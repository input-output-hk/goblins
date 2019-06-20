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
  => Gen s
  -> GoblinData Bool
  -> Maybe (Edit EditExpr)
explainGoblinGen sigGen goblin =
  ITree.treeValue
    . runMaybeT
    . distributeT
    . IGen.runGenT genSize genSeed
    $ do
        sig    <- sigGen
        newSig <- evalState (tinker sigGen) goblin
        return $ ediff sig newSig
 where
  genSize = Range.Size 1
  genSeed = Seed 12345 12345
