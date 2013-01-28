{-# LANGUAGE BangPatterns, GADTs, TypeFamilies #-}
module Data.Functor.Foldable.Attributes
    ( Cata(..)
    , CataBase
    , runCata
    , runHylo
    , mkCata
    ) where
import Control.Applicative
import Data.Bifunctor
import Data.Functor.Foldable

-- | Stores a catamorhism that produces a (hidden) value,
-- which is finally transformed to the result.
data Cata f o where
    Cata :: (f r -> r) -> (r -> o) -> Cata f o

-- | An auxiliary type alias for Foldables.
type CataBase f o = Cata (Base f) o

-- | Runs a catamorphism to produce its final result.
runCata :: (Foldable t) => CataBase t o -> (t -> o)
runCata (Cata t r) = r . cata t

-- | Runs a catamorphism combine with anamorphism.
runHylo :: (Functor f) => Cata f o -> (a -> f a) -> (a -> o)
runHylo (Cata t r) a = r . hylo t a

-- | Catamorphisms are functors.
instance Functor (Cata f) where
    fmap f (Cata c r) = Cata c (f . r)

-- | More importantly, they are applicative functors.
-- We run the two catamorphisms simultaneously and at the end, combine their
-- results.
instance Functor f => Applicative (Cata f) where
    pure  = mkCata . const
    f <*> x = fmap (uncurry ($)) (f `cataProd` x)


-- | Creates an instance of @Cata@.
mkCata :: (Functor f) => (f r -> r) -> Cata f r
mkCata f = Cata f id

-- | Creates a products of two catamorphisms.
cataProd :: (Functor f)
    => Cata f o1
    -> Cata f o2
    -> Cata f (o1, o2)
cataProd (Cata r1 o1) (Cata r2 o2) =
    Cata (\v -> let x = r1 $ fmap fst v
                    y = r2 $ fmap snd v
                 in x `seq` y `seq` (x, y))
         (bimap o1 o2)
