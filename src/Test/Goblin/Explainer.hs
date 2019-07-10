{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Goblin.Explainer where

import Test.Goblin
import Control.Monad.State.Strict (evalState)
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
  -> Maybe (Edit EditExpr)
explainGoblin sig goblin =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        newSig <- evalState (tinker (pure sig)) goblin
        return $ ediff sig newSig
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345

explainGoblinGen
  :: (Goblin Bool s, ToExpr s)
  => Maybe Size
  -> Maybe Seed
  -> Gen s
  -> GoblinData Bool
  -> Maybe (Edit EditExpr)
explainGoblinGen mbSize mbSeed sigGen goblin =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        sig    <- sigGen
        newSig <- evalState (tinker sigGen) goblin
        return $ ediff sig newSig
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
  -> IO (Maybe (Edit EditExpr))
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
