{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Goblin.Class where

import           Control.Lens
import           Control.Monad.Trans.State.Strict (State)
import           Data.Typeable (Typeable)
import           Data.TypeRepMap (TypeRepMap)
import           Hedgehog (Gen)
import           Moo.GeneticAlgorithm.Binary (bitsNeeded, decodeBinary)
import           Moo.GeneticAlgorithm.Types (Genome, Population)


data GoblinData g = GoblinData
  { -- | Remaining genes, controlling how a goblin operates.
    _genes       :: !(Genome g)
    -- | A goblin's bag of tricks contains items of many differnt types. When
    -- tinkering, a goblin (depending on its genome) might look in its bag of
    -- tricks to see whether it has anything of the appropriate type to replace
    -- what it's currently tinkering with (or, depending on the type, do
    -- something different - for example, utilise a monoid instance to add
    -- things together).
  , _bagOfTricks :: !(TypeRepMap [])
  }
makeLenses 'GoblinData

-- | Tinker monad.
type TinkerM g = State (GoblinData g)

--------------------------------------------------------------------------------
-- AddShrinks class
--------------------------------------------------------------------------------

class AddShrinks a where
  -- | Whereas `pure` creates a Hedgehog tree with no shrinks, `addShrinks`
  --   creates a tree with shrinks.
  addShrinks :: a -> Gen a
  default addShrinks
    :: Enum a
    => a
    -> Gen a
  -- TODO: add shrink tree
  addShrinks = pure

-- | Helper function to save a value in the bagOfTricks, and return it.
saveInBagOfTricks :: forall g a. Typeable a => a -> TinkerM g a
saveInBagOfTricks x = do
  bagOfTricks %= consOrInsert
  pure x
 where
  consOrInsert :: TypeRepMap [] -> TypeRepMap []
  consOrInsert trm = if TM.member @a trm
                        then TM.adjust (x:) trm
                        else TM.insert [x] trm

--------------------------------------------------------------------------------
-- SeedGoblin class & instances
--------------------------------------------------------------------------------
 
class SeedGoblin a where
  -- | Recur down a type, adding elements to the TypeRepMap
  seeder :: a -> TinkerM g ()

  default seeder
    :: (AddShrinks a, Typeable a)
    => a
    -> TinkerM g ()
  seeder x = () <$ saveInBagOfTricks x


--------------------------------------------------------------------------------
-- Gene operations
--------------------------------------------------------------------------------

-- | Read (and consume) a gene from the genome.
transcribeGene :: TinkerM g g
transcribeGene = do
  g <- use genes
  case g of
    [] -> error "Genome has run out! Try increasing the size of the genome."
    (x : xs) -> do

      genes .= xs
      return x

class GeneOps g where
  -- | Choose between two actions based on the value of a gene.
  onGene
    :: TinkerM g a
    -- ^ When gene is on.
    -> TinkerM g a
    -- ^ When gene is off.
    -> TinkerM g a

  -- | Transcribe sufficient genes to get an integer in the range [0..n].
  transcribeGenesAsInt
    :: Int
    -> TinkerM g Int


instance GeneOps Bool where

  onGene yes no = do
    tg <- transcribeGene
    if tg then yes else no

  transcribeGenesAsInt n = do
    (gs, xs) <- splitAt (bitsNeeded (0,n)) <$> use genes
    genes .= xs
    let base = n+1
    if base == 0
       then error "transcribeGenesAsInt: divide by zero"
       else return $ decodeBinary (0, n) gs `mod` base


class (GeneOps g, Typeable a) => Goblin g a where
  -- | Tinker with an item of type 'a'.
  tinker :: Gen a -> TinkerM g (Gen a)

  -- | As well as tinkering, goblins can conjure fresh items into existence.
  conjure :: TinkerM g a
