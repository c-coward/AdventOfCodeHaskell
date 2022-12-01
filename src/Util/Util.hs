-- | Common utility functions
module Util.Util where

-- | F# Style forward-application
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | Forward composition, Control.Arrow.>>> but cleaner
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f