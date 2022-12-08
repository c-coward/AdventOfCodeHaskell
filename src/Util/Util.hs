-- | Common utility functions
module Util.Util
    ( module Util.Util
    , module Control.Applicative) where

import Control.Applicative

-- | F# Style forward-application
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | Forward composition, equivalent Control.Arrow.>>>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f

infixr 1 .>, |>

(&&&+) :: (a -> b -> c) -> (a -> b -> d) -> a -> b -> (c, d)
f &&&+ g = \a b -> (f a b, g a b)