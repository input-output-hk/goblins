{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Goblin.Core
  ( module Test.Goblin.Core
  , (<$$>)
  , (<**>)
  ) where

import           Control.Arrow (first)
import           Control.Lens
import           Control.Monad (replicateM)
import           Control.Monad.Trans.State.Strict (State)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import           Data.Typeable (Typeable)
import           Data.TypeRepMap (TypeRepMap)
import qualified Data.TypeRepMap as TM
import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Language.Haskell.TH (Q, Exp, runIO, stringE)
import           Language.Haskell.TH.Syntax (addDependentFile)
import           Moo.GeneticAlgorithm.Types (Genome, Population)

import           Test.Goblin.Util


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


class (GeneOps g, Typeable a) => Goblin g a where
  -- | Tinker with an item of type 'a'.
  tinker :: Gen a -> TinkerM g (Gen a)

  -- | As well as tinkering, goblins can conjure fresh items into existence.
  conjure :: TinkerM g a


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

-- | Construct a tinker function given a set of possible things to do.
--
--   Each 'toy' is a function taking the original value and one grabbed from the
--   bag of tricks or conjured.
tinkerWithToys
  :: (AddShrinks a, Goblin g a)
  => [Gen a -> Gen a -> Gen a]
  -> (Gen a -> TinkerM g (Gen a))
tinkerWithToys toys a =
  let
    defaultToys = [const, flip const]
    allToys     = defaultToys ++ toys
  in tinkerRummagedOrConjureOrSave $ do
    toy <- (allToys !!) <$> geneListIndex allToys
    toy a <$> tinkerRummagedOrConjure

tinkerRummagedOrConjureOrSave :: (Goblin g a, AddShrinks a)
                              => TinkerM g (Gen a) -> TinkerM g (Gen a)
tinkerRummagedOrConjureOrSave m =
  onGene tinkerRummagedOrConjure (saveInBagOfTricks =<< m)

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


--------------------------------------------------------------------------------
-- Bag of tricks
--------------------------------------------------------------------------------

-- | Fetch something from the bag of tricks if there's something there.
rummage :: forall a g . (GeneOps g, Typeable a) => TinkerM g (Maybe a)
rummage = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> pure Nothing
    -- @mhueschen: \/ will not shrink, I believe
    Just xs ->
      (xs !!) <$> geneListIndex xs

-- | Fetch everything from the bag of tricks.
rummageAll :: forall a g . Typeable a => TinkerM g [a]
rummageAll = do
  bag <- use bagOfTricks
  case TM.lookup bag of
    Nothing -> pure []
    -- @mhueschen: \/ will not shrink, I believe
    Just xs -> pure xs

-- | Fetch something from the bag of tricks, or else conjure it up.
rummageOrConjure :: forall a g . Goblin g a => TinkerM g a
rummageOrConjure = maybe conjure pure =<< rummage

tinkerRummagedOrConjure :: forall a g . (Goblin g a, AddShrinks a)
                        => TinkerM g (Gen a)
tinkerRummagedOrConjure = do
  mR <- rummage
  case mR of
    Nothing -> addShrinks <$> conjure
    Just  v -> onGene (tinker v) (pure v)



-- TODO @mhueschen | make this do what the name says
applyPruneShrink :: (a -> a -> a)
                 -> Gen a -> Gen a -> Gen a
applyPruneShrink f x y = f <$> x <*> y


--------------------------------------------------------------------------------
-- Training goblins
--------------------------------------------------------------------------------

-- | Spawn a goblin from a given genome and a bag of tricks.
spawnGoblin :: Genome g -> TypeRepMap [] -> GoblinData g
spawnGoblin = GoblinData

mkEmptyGoblin :: Genome g -> GoblinData g
mkEmptyGoblin genome = GoblinData genome TM.empty


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

geneListIndex :: GeneOps g => [a] -> TinkerM g Int
geneListIndex xs = transcribeGenesAsInt (length xs - 1)


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
  addShrinks = Gen.shrink shrinkEnum . pure

shrinkEnum :: Enum a => a -> [a]
shrinkEnum x = toEnum <$>
  if e == 0
     then []
     else tail (enumFromThenTo e e1 0)
 where
  absDecr v = signum v * (abs v - 1)
  e  = fromEnum x
  e1 = absDecr e


--------------------------------------------------------------------------------
-- SeedGoblin class
--------------------------------------------------------------------------------

class SeedGoblin a where
  -- | Recur down a type, adding elements to the TypeRepMap
  seeder :: a -> TinkerM g ()

  default seeder
    :: Typeable a
    => a
    -> TinkerM g ()
  seeder x = () <$ saveInBagOfTricks x

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

decodePopulation :: BL.ByteString -> Population Bool
decodePopulation bs =
  -- The added Int tells us how much padding we must remove.
  let intermediate :: [(([Word64],Int),Double)]
      intermediate = Binary.decode bs
   in map (first splitter) intermediate

encodePopulation :: Population Bool -> BL.ByteString
encodePopulation pop =
  -- The added Int tells us how much padding we must remove.
  let intermediate :: [(([Word64],Int),Double)]
      intermediate = map (first grouper) pop
   in Binary.encode intermediate

genPopulation :: Gen (Population Bool)
genPopulation = do
  genomeSize <- Gen.int (Range.linear 0 1000)
  Gen.list (Range.linear 0 300) $ do
    genome <- replicateM genomeSize Gen.bool
    (,) <$> pure genome <*> Gen.double (Range.constant 0 (10.0^^(3::Int)))

readFirstGenomeFromFile :: FilePath -> IO [Bool]
readFirstGenomeFromFile filePath =
  (fst . head) <$> readPopulationFromFile filePath

readPopulationFromFile :: FilePath -> IO (Population Bool)
readPopulationFromFile filePath =
  decodePopulation <$> BL.readFile filePath

writePopulationToFile :: FilePath -> Population Bool -> IO ()
writePopulationToFile filePath pop =
  BL.writeFile filePath (encodePopulation pop)

loadBestPopToShownByteString :: FilePath -> Q Exp
loadBestPopToShownByteString fp = do
  addDependentFile fp
  stringE . show =<< (runIO $ do
    bs <- BL.readFile fp
    let best = head (decodePopulation bs)
    pure (encodePopulation [best]))

seedAndTinkerGenWithGenomeFromFile :: FilePath -> Q Exp -> Q Exp
seedAndTinkerGenWithGenomeFromFile fp validGen = [|
  let popStr = $(loadBestPopToShownByteString fp)
      genome = case decodePopulation (read popStr) of
                 [] -> error "sigGenChain: impossible"
                 (x,_):_ -> x
      gd = mkEmptyGoblin genome
      action = seeder env >> seeder state >> tinker $(validGen)
   in evalState action gd
   |]
