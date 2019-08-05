{-# LANGUAGE ScopedTypeVariables #-}
module Test.Goblin.Util where

import           Data.Bits (Bits(..), FiniteBits(..))
import           Data.List.Extra (chunksOf)
import           Data.Word (Word64)


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

(<$$>) :: (Functor f, Functor g)
       => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g)
       => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f x = (\g y -> g <*> y) <$> f <*> x


-- | Convert an Integral into a little-endian binary representation.
integralToBits :: (FiniteBits a) => a -> [Bool]
integralToBits x = map (testBit x) [0 .. finiteBitSize x - 1]

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

splitter :: ([Word64],Int) -> [Bool]
splitter (xs, delta) =
  let bits = concat (integralToBits <$> xs)
   in take (length bits - delta) bits

grouper :: [Bool] -> ([Word64], Int)
grouper bs =
  let (bits, delta) = padBits 64 bs
   in ( map integralFromBits (chunksOf 64 bits)
      , delta )
