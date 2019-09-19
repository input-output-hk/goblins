{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Test.Goblin.Properties where

import           Control.Monad (replicateM)
import           Data.Word (Word8, Word64)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.IO.Temp as Temp

import Test.Goblin


prop_integralBitsRoundTripWord8 :: Property
prop_integralBitsRoundTripWord8 = property $ do
  w <- forAll (Gen.integral Range.constantBounded)
  w === integralFromBits (integralToBits @Word8 w)
  bs <- forAll (replicateM 8 Gen.bool)
  bs === integralToBits @Word8 (integralFromBits bs)

prop_integralBitsRoundTripWord64 :: Property
prop_integralBitsRoundTripWord64 = property $ do
  w <- forAll (Gen.integral Range.constantBounded)
  w === integralFromBits (integralToBits @Word64 w)
  bs <- forAll (replicateM 64 Gen.bool)
  bs === integralToBits @Word64 (integralFromBits bs)

prop_SplitGroupRoundtrip :: Property
prop_SplitGroupRoundtrip = property $ do
  bits <- forAll (Gen.list (Range.linear 0 300) Gen.bool)
  bits === splitter (grouper bits)

prop_PopulationRoundtrip :: Property
prop_PopulationRoundtrip = property $ do
  pop <- forAll genPopulation
  pop === decodePopulation (encodePopulation pop)

prop_PopulationRoundtripViaFile :: Property
prop_PopulationRoundtripViaFile = property $ do
  pop <- forAll genPopulation
  fp <- evalIO (Temp.emptySystemTempFile "prop_file")
  () <- evalIO $ writePopulationToFile fp pop
  pop' <- evalIO $ readPopulationFromFile fp
  pop === pop'

tests :: Group
tests = $$(discover)
