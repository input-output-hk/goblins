{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Goblin where

import           Control.Applicative (liftA, liftA2)
import           Control.Lens
import           Control.Monad (liftM, replicateM)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Control (MonadTransControl(..))
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Char (chr)
import           Data.Int
import           Data.List (splitAt)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Ratio (Ratio, (%), numerator, denominator)
import           Data.Typeable (Typeable)
import           Data.TypeRepMap (TypeRepMap)
import qualified Data.TypeRepMap as TM
import           Data.Word (Word64)
import           GHC.Generics
import           Hedgehog (Gen, MonadGen(..))
import           Hedgehog.Internal.Gen (GenT(..), mapGenT)
import           Hedgehog.Internal.Range (Size)
import           Hedgehog.Internal.Seed (Seed)
import           Hedgehog.Internal.Tree (Tree, TreeT(..))
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Distributive
  (MonadTransDistributive(..), MonadTransJuggle(..))
import qualified Hedgehog.Range as Range
import           Moo.GeneticAlgorithm.Binary (bitsNeeded, decodeBinary)
import           Moo.GeneticAlgorithm.Types (Genome)
import           Numeric.Natural (Natural)


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
  :: (AddShrinks a, Goblin g a, Typeable a, GeneOps g)
  => [Gen a -> Gen a -> Gen a]
  -> (Gen a -> TinkerM g (Gen a))
tinkerWithToys toys =
  let
    defaultToys = [const, flip const]
    allToys     = defaultToys ++ toys
  in \a -> do
    toy <- (allToys !!) <$> geneListIndex allToys
    toy a <$> (addShrinks <$> rummageOrConjure)

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
rummageOrConjure :: forall a g . (Typeable a, Goblin g a) => TinkerM g a
rummageOrConjure = maybe conjure pure =<< rummage

--------------------------------------------------------------------------------
-- Generic goblins
--------------------------------------------------------------------------------

{-

so I think I need to perform operations over entire trees here. I'm not sure
that my tinker type is actually right. maybe it should consume a GenT (TreeT)
and return one (wrapped in state) as well.

then it can perform operations on all parts of the tree. right now we're
treating it, oddly, like a regular monad - we expect to bind single variables
and return them in an `m a` context. however here we want to "leave them in
their boxes" and work `m a -> m a`.

-}

{- going to leave out generics until I better understand what is going on

class GGoblin g f where
  gTinker :: f a -> TinkerM g (f a)
  gConjure :: TinkerM g (f a)

-- gRummageOrConjure
--   :: (Generic a, Goblin g a, GGoblin g f, f ~ Rep a)
--   => TinkerM g (f a)
-- gRummageOrConjure = GHC.Generics.from <$> rummageOrConjure

instance GGoblin g V1 where
  gTinker = return
  gConjure = error "Cannot conjure a void type"

instance GGoblin g U1 where
  gTinker = return
  gConjure = return U1

instance Goblin g c => GGoblin g (K1 i c) where
  gTinker x = K1 <$> tinker (unK1 x)
  gConjure = K1 <$> conjure

instance GGoblin g f => GGoblin g (M1 i t f) where
  gTinker (M1 x) = M1 <$> gTinker x
  gConjure = M1 <$> gConjure

-- TODO In the 'tinker' implementations here, we would like to `rummageOrConjure`
-- rather than just conjuring when we switch to the other branch
instance (GeneOps g, GGoblin g a, GGoblin g b) => GGoblin g (a :+: b) where
  gTinker (L1 x) = onGene (R1 <$> gConjure) (L1 <$> gTinker x)
  gTinker (R1 x) = onGene (L1 <$> gConjure) (R1 <$> gTinker x)

  gConjure = onGene (L1 <$> gConjure) (R1 <$> gConjure)

instance (GeneOps g, GGoblin g a, GGoblin g b) => GGoblin g (a :*: b) where
  gTinker (a :*: b) = liftA2 (:*:) (onGene (gTinker a) (return a)) (onGene (gTinker b) (return b))
  gConjure = liftA2 (:*:) gConjure gConjure

-}

--------------------------------------------------------------------------------
-- Primitive goblins
--------------------------------------------------------------------------------

instance GeneOps a => Goblin a Bool where
  tinker b = addShrinks <$> onGene rummageOrConjure conjure
  conjure = saveInBagOfTricks =<< onGene (pure True) (pure False)

instance GeneOps a => Goblin a Char where
  tinker b = addShrinks <$> onGene rummageOrConjure conjure
  -- TODO : this uses up 21 bits of genome, and we may not be interested in thorough
  -- coverage of the Char space
  conjure = saveInBagOfTricks =<< chr <$> transcribeGenesAsInt 1114111

instance GeneOps a => Goblin a Integer where
  tinker = tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< toEnum <$> conjure

instance GeneOps a => Goblin a Natural where
  tinker = tinkerWithToys (map applyPruneShrink [(+), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Int where
  tinker = tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< (\x -> x-1000) <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Word64 where
  tinker = tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000


-- TODO @mhueschen | make this do what the name says
applyPruneShrink :: (a -> a -> a)
                 -> Gen a -> Gen a -> Gen a
applyPruneShrink f x y = f <$> x <*> y

--------------------------------------------------------------------------------
-- Composite goblins
--------------------------------------------------------------------------------

instance (Goblin g a, Goblin g b) => Goblin g (a,b) where
  tinker p = do
    x <- tinker (fst <$> p)
    y <- tinker (snd <$> p)
    pure ((,) <$> x <*> y)

  conjure = saveInBagOfTricks =<< (,) <$> conjure <*> conjure

instance (Integral a, Goblin g a) => Goblin g (Ratio a) where
  tinker obj = do
    n <- tinker (numerator <$> obj)
    d <- tinker (denominator <$> obj)
    pure ((%) <$> n <*> d)

  conjure = saveInBagOfTricks =<< (%) <$> conjure <*> conjure

instance Goblin g a => Goblin g (Maybe a) where
  -- TODO mhueschen - reconsider this. it seems suspect.
  tinker obj = do
    x <- tinker (Gen.just obj)
    pure (Gen.maybe x)

  conjure = saveInBagOfTricks =<<
    onGene (pure Nothing) (Just <$> conjure)

-- | Our list goblin behaves slightly differently, since it pulls whole lists of
-- things from the bag of tricks, and is also specialised to do some more
-- messing about with lists.
instance (AddShrinks a, Eq a, Typeable a, GeneOps g, Goblin g a)
      => Goblin g [a] where
  tinker obj = do
    rummaged <- (map (sequenceA . map addShrinks)) <$> rummageAll
    -- If there's nothing to rummage, we do unary operations
    -- Otherwise we select BinOps or UnOps based on a gene
    if (null rummaged)
       then toyUnOp
       else onGene toyUnOp (toyBinOp rummaged)

   where
    toyUnOp :: TinkerM g (Gen [a])
    toyUnOp = do
      toy <- (unOpToys !!) <$> geneListIndex unOpToys
      toy obj

    unOpToys :: [Gen [a] -> TinkerM g (Gen [a])]
    unOpToys =
      [ pure
      , \a -> pure (Gen.shuffle =<< a)
      , \a -> pure (Gen.subsequence =<< a)
      -- TODO mhueschen | consider tinkering with elements here
      ]

    toyBinOp :: [Gen [a]] -> TinkerM g (Gen [a])
    toyBinOp rummaged = do
      toy <- (binOpToys !!) <$> geneListIndex binOpToys
      val <- (rummaged !!) <$> geneListIndex rummaged
      toy obj val

    -- Toys for lists can use 'TinkerM', because they might be random
    binOpToys :: Eq a => [Gen [a] -> Gen [a] -> TinkerM g (Gen [a])]
    binOpToys =
      [ \a _ -> pure a
      , \_ b -> pure b
      , \a _ -> pure (Gen.shuffle =<< a)
      , \a b -> pure ((++) <$> a <*> (Gen.subsequence =<< b))
      , \a b -> pure ((List.\\) <$> a <*> (Gen.subsequence =<< b))
      , \a b -> pure ((++) <$> (Gen.subsequence =<< a)
                           <*> (Gen.subsequence =<< b))
      ]

  conjure = saveInBagOfTricks =<< do
    listLen <- transcribeGenesAsInt 15
    replicateM listLen conjure

instance (Goblin g a, Ord a, AddShrinks a, Typeable a)
      => Goblin g (Set.Set a) where
  tinker obj = do
    rummaged <- (map (sequenceA . map addShrinks)) <$> rummageAll
    -- If there's nothing to rummage, we do unary operations
    -- Otherwise we select BinOps or UnOps based on a gene
    if (null rummaged)
       then toyUnOp
       else onGene toyUnOp (toyBinOp rummaged)

   where
    toyUnOp :: TinkerM g (Gen (Set.Set a))
    toyUnOp = do
      toy <- (unOpToys !!) <$> geneListIndex unOpToys
      toy obj

    unOpToys :: [Gen (Set.Set a) -> TinkerM g (Gen (Set.Set a))]
    unOpToys =
      [ \a -> pure a
      , \a -> pure (Set.fromList <$> (Gen.shuffle =<< (Set.toList <$> a)))
      , \a -> pure (Set.fromList <$> (Gen.subsequence =<< (Set.toList <$> a)))
      -- TODO mhueschen | consider tinkering with elements here
      ]

    toyBinOp :: [Gen [a]] -> TinkerM g (Gen (Set.Set a))
    toyBinOp rummaged = do
      toy <- (binOpToys !!) <$> geneListIndex binOpToys
      val <- (rummaged !!) <$> geneListIndex rummaged
      toy obj val

    -- Toys for sets can use 'TinkerM', because they might be random
    binOpToys :: [Gen (Set.Set a) -> Gen [a] -> TinkerM g (Gen (Set.Set a))]
    binOpToys =
      [ \a _ -> pure a
      , \_ b -> pure (Set.fromList <$> b)
      , \a b -> pure (Set.difference
                       <$> a
                       <*> (Set.fromList <$> (Gen.subsequence =<< b)))
      , \a b -> pure (Set.union
                       <$> a
                       <*> (Set.fromList <$> (Gen.subsequence =<< b)))
      , \a b -> pure $ (Set.intersection <$> a <*> (Set.fromList <$> b))
      ]

  conjure = saveInBagOfTricks =<< do
    listLen <- transcribeGenesAsInt 15
    cs <- replicateM listLen conjure
    pure (Set.fromList cs)

instance (Goblin g k, Goblin g v, Ord k, Eq k, Eq v,
          AddShrinks k, AddShrinks v, Typeable k, Typeable v)
         => Goblin g (Map.Map k v) where
    tinker obj = do
      rummagedKeys <- (map (sequenceA . map addShrinks)) <$> rummageAll
      rummagedVals <- (map (sequenceA . map addShrinks)) <$> rummageAll
      -- If there's nothing to rummage, we do unary operations
      -- Otherwise we select BinOps or UnOps based on a gene
      if (null rummagedKeys) || (null rummagedVals)
         then toyUnOp
         else onGene toyUnOp (toyBinOp rummagedKeys rummagedVals)

     where
      toyUnOp :: TinkerM g (Gen (Map.Map k v))
      toyUnOp = do
        toy <- (unOpToys !!) <$> geneListIndex unOpToys
        toy obj

      unOpToys :: [Gen (Map.Map k v) -> TinkerM g (Gen (Map.Map k v))]
      unOpToys =
        [ \a -> pure a
        , \a -> pure (Map.fromList <$> (Gen.shuffle =<< (Map.toList <$> a)))
        , \a -> pure (Map.fromList <$> (Gen.subsequence =<< (Map.toList <$> a)))
        -- TODO mhueschen | consider tinkering with elements here
        ]

      toyBinOp :: [Gen [k]] -> [Gen [v]] -> TinkerM g (Gen (Map.Map k v))
      toyBinOp rummagedKeys rummagedVals = do
        toy <- (binOpToys !!) <$> geneListIndex binOpToys
        key <- (rummagedKeys !!) <$> geneListIndex rummagedKeys
        val <- (rummagedVals !!) <$> geneListIndex rummagedVals
        toy obj key val

      -- Toys for sets can use 'TinkerM', because they might be random
      binOpToys :: [Gen (Map.Map k v) -> Gen [k] -> Gen [v]
                -> TinkerM g (Gen (Map.Map k v))]
      binOpToys =
        [ \a _ _ -> pure a
        , \a k v ->
          pure (Map.union
                 <$> a
                 <*> (Map.fromList <$> (zip <$> (Gen.subsequence =<< k)
                                            <*> (Gen.subsequence =<< v))))
        , \a k _ -> pure (Map.withoutKeys <$> a
                           <*> (Set.fromList <$> (Gen.subsequence =<< k)))
        , \a k _ -> pure (Map.restrictKeys <$> a
                           <*> (Set.fromList <$> (Gen.subsequence =<< k)))
        ]

    conjure = saveInBagOfTricks =<< do
      listLen <- transcribeGenesAsInt 15
      cs <- replicateM listLen conjure
      pure (Map.fromList cs)


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
  -- TODO: add shrink tree
  addShrinks = pure

instance AddShrinks Bool where
instance AddShrinks Char where
instance AddShrinks Integer where
instance AddShrinks Natural where
instance AddShrinks Int where
instance AddShrinks Word64 where
