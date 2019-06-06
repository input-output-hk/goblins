{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Goblin.Explainer where

import Test.Goblin
import Control.Monad.State.Strict (runState)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor.Identity (runIdentity)
import Data.TreeDiff
import Data.TreeDiff.Class
import qualified Data.TypeRepMap as TM
import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree
import           Moo.GeneticAlgorithm.Types (Population)

explainGoblin
  :: (Goblin Bool s, ToExpr s)
  => s
  -> GoblinData Bool
  -> Maybe (Edit EditExpr, GoblinData Bool)
explainGoblin sig goblin =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        let (newSigGen, finalGoblin) = runState (tinker (pure sig)) goblin
        newSig <- newSigGen
        pure $ (ediff sig newSig, finalGoblin)
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345

explainGoblinGen
  :: (Goblin Bool s, ToExpr s)
  => Maybe Size
  -> Maybe Seed
  -> Gen s
  -> GoblinData Bool
  -> Maybe (s, s, Edit EditExpr, GoblinData Bool)
explainGoblinGen mbSize mbSeed sigGen goblin =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        sig    <- sigGen
        let (newSigGen, finalGoblin) = runState (tinker (pure sig)) goblin
        newSig <- newSigGen
        pure $ (sig, newSig, ediff sig newSig, finalGoblin)
 where
  genSize = case mbSize of
              Nothing -> Range.Size 1
              Just sz -> sz
  genSeed = case mbSeed of
              Nothing -> Seed 12345 12345
              Just sd -> sd

explainGoblinGenFromFile
  :: (Goblin Bool s, ToExpr s)
  => Maybe Size
  -> Maybe Seed
  -> Gen s
  -> FilePath
  -> IO (Maybe (s, s, Edit EditExpr, GoblinData Bool))
explainGoblinGenFromFile mbSize mbSeed sigGen fp = do
  str <- readFile fp
  pop <- case reads str :: [(Population Bool,String)] of
           [(pop,"")] -> pure pop
           _          -> error ("couldn't parse file: " <> fp)
  let bestGenome = case pop of
                     [] -> error "empty population"
                     ((best,_score):_) -> best
  let goblin = mkEmptyGoblin bestGenome
  pure (explainGoblinGen mbSize mbSeed sigGen goblin)
