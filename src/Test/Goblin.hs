-- | The top-level module of Goblins, which re-exports the main functionality.
module Test.Goblin
  ( module Test.Goblin.Core
  , module Test.Goblin.Explainer
  , module Test.Goblin.Persist
  ) where

import Test.Goblin.Core
import Test.Goblin.Explainer
import Test.Goblin.Instances ()
import Test.Goblin.Persist
