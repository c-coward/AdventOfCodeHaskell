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

rep f a = f a a

data Pair a = P {y :: a, x :: a} deriving (Show, Eq)
instance Functor Pair where
    fmap f (P a b) = P (f a) (f b)
instance Applicative Pair where
    pure = rep P
    P f g <*> P a b = P (f a) (g b)
instance Num a => Num (Pair a) where
    a + b = (+) <$> a <*> b
    a - b = (-) <$> a <*> b
    a * b = (*) <$> a <*> b
    abs = (abs <$>)
    signum = (signum <$>)
    fromInteger = pure . fromInteger