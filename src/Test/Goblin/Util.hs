module Test.Goblin.Util where


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

(<$$>) :: (Functor f, Functor g)
       => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<**>) :: (Applicative f, Applicative g)
       => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) f x = (\g y -> g <*> y) <$> f <*> x
