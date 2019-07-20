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
import qualified Data.Bimap as Bimap
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
import           Data.Word (Word8, Word64)
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

tinkerRummagedOrConjure :: forall a g . (Typeable a, Goblin g a, AddShrinks a)
                        => TinkerM g (Gen a)
tinkerRummagedOrConjure = do
  mR <- rummage
  case mR of
    Nothing -> addShrinks <$> conjure
    Just  v -> onGene (tinker v) (pure v)


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
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< toEnum <$> conjure

instance GeneOps a => Goblin a Natural where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map applyPruneShrink [(+), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Int where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< (\x -> x-1000) <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Word64 where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

-- | This instance generates Double values in range [0..1] (inclusive) at 0.01
-- increments. 0.01, 0.02 ... 0.99, 1.00
instance GeneOps a => Goblin a Double where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map applyPruneShrink [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< do
    i <- transcribeGenesAsInt 100
    pure (fromIntegral i / 100)


-- TODO @mhueschen | make this do what the name says
applyPruneShrink :: (a -> a -> a)
                 -> Gen a -> Gen a -> Gen a
applyPruneShrink f x y = f <$> x <*> y

--------------------------------------------------------------------------------
-- Composite goblins
--------------------------------------------------------------------------------

instance (Goblin g a, Goblin g b, AddShrinks a, AddShrinks b)
      => Goblin g (a,b) where
  tinker p = tinkerRummagedOrConjureOrSave $ do
    x <- tinker (fst <$> p)
    y <- tinker (snd <$> p)
    pure ((,) <$> x <*> y)

  conjure = saveInBagOfTricks =<< (,) <$> conjure <*> conjure

instance (Goblin g a, Goblin g b, Goblin g c, AddShrinks a, AddShrinks b, AddShrinks c)
      => Goblin g (a,b,c) where
  tinker p = tinkerRummagedOrConjureOrSave $ do
    x <- tinker ((\(x,_,_) -> x) <$> p)
    y <- tinker ((\(_,x,_) -> x) <$> p)
    z <- tinker ((\(_,_,x) -> x) <$> p)
    pure ((\x y z -> (x,y,z)) <$> x <*> y <*> z)

  conjure = saveInBagOfTricks =<<
    (\x y z -> (x,y,z)) <$> conjure <*> conjure <*> conjure

instance (Integral a, Goblin g a, AddShrinks a)
      => Goblin g (Ratio a) where
  tinker obj = tinkerRummagedOrConjureOrSave $ do
    n <- tinker (numerator <$> obj)
    d <- tinker (denominator <$> obj)
    pure ((%) <$> n <*> d)

  conjure = saveInBagOfTricks =<< (%) <$> conjure <*> conjure

instance (Goblin g a, AddShrinks a)
      => Goblin g (Maybe a) where
  -- TODO mhueschen - reconsider this. it seems suspect.
  tinker obj = tinkerRummagedOrConjureOrSave $ do
    x <- tinker (Gen.just obj)
    pure (Gen.maybe x)

  conjure = saveInBagOfTricks =<<
    onGene (pure Nothing) (Just <$> conjure)

-- | Our list goblin behaves slightly differently, since it pulls whole lists of
-- things from the bag of tricks, and is also specialised to do some more
-- messing about with lists.
instance (AddShrinks a, Eq a, Typeable a, GeneOps g, Goblin g a)
      => Goblin g [a] where
  tinker obj = tinkerRummagedOrConjureOrSave $ do
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
  tinker obj = tinkerRummagedOrConjureOrSave $ do
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

instance (Goblin g k, Goblin g v, Ord k, Eq k, Eq v, AddShrinks (Map.Map k v),
          AddShrinks k, AddShrinks v, Typeable k, Typeable v)
         => Goblin g (Map.Map k v) where
    tinker obj = tinkerRummagedOrConjureOrSave $ do
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
instance AddShrinks Double where
instance AddShrinks Integer where
instance AddShrinks Natural where
instance AddShrinks Int where
instance AddShrinks Word8 where
instance AddShrinks Word64 where
instance (AddShrinks a, AddShrinks b) => AddShrinks (a, b) where
  addShrinks = pure
instance (AddShrinks a, AddShrinks b, AddShrinks c) => AddShrinks (a, b, c) where
  addShrinks = pure
instance (AddShrinks k, AddShrinks v) => AddShrinks (Map.Map k v) where
  addShrinks = pure
instance AddShrinks a => AddShrinks [a] where
  addShrinks = pure
instance AddShrinks a => AddShrinks (Set.Set a) where
  addShrinks = pure
instance AddShrinks a => AddShrinks (Maybe a) where
  addShrinks = pure
instance AddShrinks a => AddShrinks (Ratio a) where
  addShrinks = pure


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

instance SeedGoblin Bool where
instance SeedGoblin Char where
instance SeedGoblin Integer where
instance SeedGoblin Natural where
instance SeedGoblin Int where
instance SeedGoblin Word8 where
instance SeedGoblin Word64 where
instance SeedGoblin Double where

instance (SeedGoblin a, Typeable a) => SeedGoblin [a] where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> xs)
instance (SeedGoblin a, Typeable a, SeedGoblin b, Typeable b)
  => SeedGoblin (Bimap.Bimap a b) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Bimap.keys xs)
    () <$ sequenceA (seeder <$> Bimap.elems xs)
instance (SeedGoblin a, Typeable a, SeedGoblin b, Typeable b)
  => SeedGoblin (Map.Map a b) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Map.keys xs)
    () <$ sequenceA (seeder <$> Map.elems xs)
instance (SeedGoblin a, Typeable a) => SeedGoblin (Set.Set a) where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> Set.toList xs)
instance (SeedGoblin a, Typeable a, SeedGoblin b, Typeable b)
  => SeedGoblin (a,b) where
  seeder a@(x,y) = do
    () <$ saveInBagOfTricks a
    () <$ seeder x
    () <$ seeder y
instance (SeedGoblin a, Typeable a, SeedGoblin b, Typeable b, SeedGoblin c, Typeable c)
  => SeedGoblin (a,b,c) where
  seeder a@(x,y,z) = do
    () <$ saveInBagOfTricks a
    () <$ seeder x
    () <$ seeder y
    () <$ seeder z
instance ( SeedGoblin x1, Typeable x1
         , SeedGoblin x2, Typeable x2
         , SeedGoblin x3, Typeable x3
         , SeedGoblin x4, Typeable x4 )
  => SeedGoblin (x1,x2,x3,x4) where
  seeder a@(x1,x2,x3,x4) = do
    () <$ saveInBagOfTricks a
    () <$ seeder x1
    () <$ seeder x2
    () <$ seeder x3
    () <$ seeder x4
instance ( SeedGoblin x1, Typeable x1
         , SeedGoblin x2, Typeable x2
         , SeedGoblin x3, Typeable x3
         , SeedGoblin x4, Typeable x4
         , SeedGoblin x5, Typeable x5
         , SeedGoblin x6, Typeable x6
         , SeedGoblin x7, Typeable x7
         , SeedGoblin x8, Typeable x8
         , SeedGoblin x9, Typeable x9 )
  => SeedGoblin (x1,x2,x3,x4,x5,x6,x7,x8,x9) where
  seeder a@(x1,x2,x3,x4,x5,x6,x7,x8,x9) = do
    () <$ saveInBagOfTricks a
    () <$ seeder x1
    () <$ seeder x2
    () <$ seeder x3
    () <$ seeder x4
    () <$ seeder x5
    () <$ seeder x6
    () <$ seeder x7
    () <$ seeder x8
    () <$ seeder x9


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

(<$$>) :: (Functor f, Functor g)
       => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g)
       => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f x = (\g y -> g <*> y) <$> f <*> x
