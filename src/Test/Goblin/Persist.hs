{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Utilities for reading from / writing to the filesystem
module Test.Goblin.Persist where

import           Control.Arrow (first)
import qualified Data.Binary as Binary
import           Data.Bits (Bits(..), FiniteBits(..))
import qualified Data.ByteString.Lazy as BL
import           Data.List.Extra (chunksOf)
import           Data.Word (Word64)
import           Language.Haskell.TH (Q, Exp, runIO, stringE)
import           Moo.GeneticAlgorithm.Types (Population)


-- | Decode a `Population Bool` from a lazy ByteString.
decodePopulation :: BL.ByteString -> Population Bool
decodePopulation bs =
  -- The added Int tells us how much padding we must remove.
  let intermediate :: [(([Word64],Int),Double)]
      intermediate = Binary.decode bs
   in map (first splitter) intermediate

-- | Encode a `Population Bool` to a lazy ByteString.
encodePopulation :: Population Bool -> BL.ByteString
encodePopulation pop =
  -- The added Int tells us how much padding we must remove.
  let intermediate :: [(([Word64],Int),Double)]
      intermediate = map (first grouper) pop
   in Binary.encode intermediate

-- | Load a Population from a file and return the first (highest
-- scoring) genome.
readFirstGenomeFromFile :: FilePath -> IO [Bool]
readFirstGenomeFromFile filePath =
  (fst . head) <$> readPopulationFromFile filePath

-- | Read a Population from a file.
readPopulationFromFile :: FilePath -> IO (Population Bool)
readPopulationFromFile filePath =
  decodePopulation <$> BL.readFile filePath

-- | Write a Population to a file.
writePopulationToFile :: FilePath -> Population Bool -> IO ()
writePopulationToFile filePath pop =
  BL.writeFile filePath (encodePopulation pop)

-- | Read a file at compile-time, and splice in the `show` of its ByteString
-- as a String in the source file.
loadBestPopToShownByteString :: FilePath -> Q Exp
loadBestPopToShownByteString fp = do
  stringE . show =<< (runIO $ do
    bs <- BL.readFile fp
    let best = head (decodePopulation bs)
    pure (encodePopulation [best]))

-- | Splice in a genome as a `show`n ByteString, and decode it at runtime.
-- This is less safe than inlining the whole list of `Bool`s into source code,
-- but results in less source bloat.
loadGoblinDataFromFilePath :: FilePath
                           -> Q Exp
loadGoblinDataFromFilePath fp = [|
  let popStr = $(loadBestPopToShownByteString fp)
      genome = case decodePopulation (read popStr) of
                 [] -> error "sigGenChain: impossible"
                 (x,_):_ -> x
   in mkEmptyGoblin genome
   |]

-- | Convert an Integral into a little-endian binary representation.
integralToBits :: (FiniteBits a) => a -> [Bool]
integralToBits x = map (testBit x) [0 .. finiteBitSize x - 1]

-- | Convert from a little-endian binary representation to an Integral.
integralFromBits :: forall a. (Integral a, FiniteBits a) => [Bool] -> a
integralFromBits bits =
  if length bits `mod` numBits /= 0
     then error ("length of bits is not a multiple of " <> show numBits)
     else sum (zipWith (\b i -> if b then 2^i else 0)
              bits
              [0 .. numBits])
 where
  numBits = finiteBitSize (undefined :: a)

-- | Returns the padded list, plus the number of padding bits added.
padBits :: Int -> [Bool] -> ([Bool], Int)
padBits multiple bs = ( bs ++ replicate delta False
                      , delta )
 where
  delta = multiple - (length bs `rem` multiple)

-- | Split a list of `Word64`s, little-endian style, into their requisite
-- bits. The `Int` describes the amount of padding to drop, since we must
-- necessarily pad up to a 64-bit multiple.
splitter :: ([Word64],Int) -> [Bool]
splitter (xs, delta) =
  let bits = concat (integralToBits <$> xs)
   in take (length bits - delta) bits

-- | Group a list of `Bool`s into a list of `Word64`s, little-endian style.
-- The `Int` describes the amount of padding added, since we must necessarily
-- pad up to a 64-bit multiple.
grouper :: [Bool] -> ([Word64], Int)
grouper bs =
  let (bits, delta) = padBits 64 bs
   in ( map integralFromBits (chunksOf 64 bits)
      , delta )
