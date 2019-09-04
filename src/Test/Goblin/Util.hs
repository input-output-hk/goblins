-- | Utility functions
module Test.Goblin.Util where


-- | `<$>` through nested functors.
(<$$>) :: (Functor f, Functor g)
       => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- | `<*>` through nested functors.
(<**>) :: (Applicative f, Applicative g)
       => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f x = (\g y -> g <*> y) <$> f <*> x
