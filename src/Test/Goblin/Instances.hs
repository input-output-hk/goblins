{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Goblin.Instances where

import           Control.Applicative         (liftA2)
import           Control.Monad               (replicateM)
import qualified Data.Bimap                  as Bimap
import           Data.Char                   (chr)
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import           Data.Ratio                  (Ratio)
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import           Data.Typeable               (Typeable)
import           Data.Word                   (Word64, Word8)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Gen                as Gen
import           Lens.Micro.Mtl              (use, (.=))
import           Moo.GeneticAlgorithm.Binary (bitsNeeded, decodeBinary)
import           Numeric.Natural             (Natural)

import           Test.Goblin.Core
import           Test.Goblin.TH


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
-- AddShrinks
--------------------------------------------------------------------------------

instance AddShrinks () where
instance AddShrinks Bool where
instance AddShrinks Char where
instance AddShrinks Double where
instance AddShrinks Integer where
instance AddShrinks Natural where
instance AddShrinks Int where
instance AddShrinks Word8 where
instance AddShrinks Word64 where

deriveAddShrinks ''(,)
deriveAddShrinks ''(,,)
deriveAddShrinks ''(,,,)
deriveAddShrinks ''(,,,,)
deriveAddShrinks ''(,,,,,)
deriveAddShrinks ''(,,,,,,)
deriveAddShrinks ''(,,,,,,,)
deriveAddShrinks ''(,,,,,,,,)
deriveAddShrinks ''(,,,,,,,,,)
deriveAddShrinks ''(,,,,,,,,,,)
deriveAddShrinks ''Ratio

instance (AddShrinks k, Ord k, AddShrinks v) => AddShrinks (Map.Map k v) where
  addShrinks xs = Map.fromList <$> mapM addShrinks (Map.toList xs)

instance AddShrinks a => AddShrinks [a] where
  addShrinks ls = mapM addShrinks ls

instance (AddShrinks a, Ord a) => AddShrinks (Set.Set a) where
  addShrinks xs = Set.fromList <$> mapM addShrinks (Set.toList xs)

instance AddShrinks a => AddShrinks (Maybe a) where
  addShrinks Nothing  = pure Nothing
  addShrinks (Just x) = Just <$> addShrinks x

--------------------------------------------------------------------------------
-- Primitive goblins
--------------------------------------------------------------------------------

instance GeneOps a => Goblin a Bool where
  tinker _ = addShrinks <$> onGene rummageOrConjure conjure
  conjure = saveInBagOfTricks =<< onGene (pure True) (pure False)

instance GeneOps a => Goblin a Char where
  tinker _ = addShrinks <$> onGene rummageOrConjure conjure
  -- TODO : this uses up 21 bits of genome, and we may not be interested in thorough
  -- coverage of the Char space
  conjure = saveInBagOfTricks =<< chr <$> transcribeGenesAsInt 1114111

instance GeneOps a => Goblin a Integer where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< toEnum <$> conjure

instance GeneOps a => Goblin a Natural where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Int where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< (\x -> x-1000) <$> transcribeGenesAsInt 2000

instance GeneOps a => Goblin a Word64 where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< fromIntegral <$> transcribeGenesAsInt 2000

-- | This instance generates Double values in range [0..1] (inclusive) at 0.01
-- increments. 0.01, 0.02 ... 0.99, 1.00
instance GeneOps a => Goblin a Double where
  tinker = tinkerRummagedOrConjureOrSave
           . tinkerWithToys (map liftA2 [(+), (-), (*)])
  conjure = saveInBagOfTricks =<< do
    i <- transcribeGenesAsInt 100
    pure (fromIntegral i / 100)


--------------------------------------------------------------------------------
-- Composite goblins
--------------------------------------------------------------------------------

deriveGoblin ''(,)
deriveGoblin ''(,,)
deriveGoblin ''(,,,)
deriveGoblin ''(,,,,)
deriveGoblin ''(,,,,,)
deriveGoblin ''(,,,,,,)
deriveGoblin ''(,,,,,,,)
deriveGoblin ''(,,,,,,,,)
deriveGoblin ''(,,,,,,,,,)
deriveGoblin ''(,,,,,,,,,,)
deriveGoblin ''Ratio

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
instance (AddShrinks a, Eq a, Goblin g a)
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
    binOpToys :: [Gen [a] -> Gen [a] -> TinkerM g (Gen [a])]
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

instance (Goblin g a, Ord a, AddShrinks a)
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

instance (Goblin g k, Goblin g v, Ord k, Eq v, AddShrinks k, AddShrinks v)
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
-- SeedGoblin
--------------------------------------------------------------------------------

instance SeedGoblin () where
instance SeedGoblin Bool where
instance SeedGoblin Char where
instance SeedGoblin Integer where
instance SeedGoblin Natural where
instance SeedGoblin Int where
instance SeedGoblin Word8 where
instance SeedGoblin Word64 where
instance SeedGoblin Double where

deriveSeedGoblin ''(,)
deriveSeedGoblin ''(,,)
deriveSeedGoblin ''(,,,)
deriveSeedGoblin ''(,,,,)
deriveSeedGoblin ''(,,,,,)
deriveSeedGoblin ''(,,,,,,)
deriveSeedGoblin ''(,,,,,,,)
deriveSeedGoblin ''(,,,,,,,,)
deriveSeedGoblin ''(,,,,,,,,,)
deriveSeedGoblin ''(,,,,,,,,,,)

instance (SeedGoblin a, Typeable a) => SeedGoblin [a] where
  seeder xs = do
    () <$ saveInBagOfTricks xs
    () <$ sequenceA (seeder <$> xs)
instance (SeedGoblin a, Typeable a) => SeedGoblin (Seq.Seq a) where
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
instance (SeedGoblin a, Typeable a) => SeedGoblin (Maybe a) where
  seeder mb = do
    () <$ saveInBagOfTricks mb
    case mb of
      Nothing -> pure ()
      Just x  -> seeder x
