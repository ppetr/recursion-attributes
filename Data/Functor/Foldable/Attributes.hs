{-# LANGUAGE GADTs, TypeFamilies #-}
module Data.Functor.Foldable.Attributes
{-
    ( Cata(..)
    , CataBase
    , runCata
    , runHylo
    , mkCata
    , mkCata1
    , mkCata2
    , mkCata3
    ) -} where
import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
--import Data.Bifunctor
import Data.Functor.Foldable

-- | Stores a catamorhism that produces a (hidden) value,
-- which is finally transformed to the result.
data Cata f i o where
    Cata :: (i -> f r -> r) -> (i -> r -> o) -> Cata f i o

-- | An auxiliary type alias for Foldables.
type CataBase f i o = Cata (Base f) i o

-- | Runs a catamorphism to produce its final result.
runCata :: (Foldable t) => CataBase t () o -> (t -> o)
runCata (Cata t r) = r () . cata (t ())

-- | Runs a catamorphism combine with anamorphism.
runHylo :: (Functor f) => Cata f () o -> (a -> f a) -> (a -> o)
runHylo (Cata t r) a = r () . hylo (t ()) a

-- | Catamorphisms are functors.
instance Functor (Cata f i) where
    fmap f (Cata c r) = Cata c ((f .) . r)
    {-# INLINE fmap #-}

instance Functor f => C.Category (Cata f) where
    id = arr id
    {-# INLINE id #-}
    (Cata r1 o1) . (Cata r2 o2) =
        Cata (\i v -> let y = r2 i        $ fmap snd v
                          x = r1 (o2 i y) $ fmap fst v
                       in x `seq` y `seq` (x, y))
             (\i v -> o1 (o2 i $ snd v) $ fst v)
    {-# INLINE (.) #-}
instance Functor f => Arrow (Cata f) where
    arr g = Cata (\x _ -> g x) (const id)
    {-# INLINE arr #-}
    first (Cata r o) =
        Cata (\(i, _) v -> r i v)
             (\i x -> (o (fst i) x, snd i))
    {-# INLINE first #-}

-- | More importantly, they are applicative functors.
-- We run the two catamorphisms simultaneously and at the end, combine their
-- results.
instance Functor f => Applicative (Cata f i) where
    pure  = mkCata' . const
    {-# INLINE pure #-}
    f <*> x = (f &&& x) >>^ uncurry ($)
    {-# INLINE (<*>) #-}

-- | Creates an instance of @Cata@ from an unparametrized catamorphism.
mkCata' :: (Functor f) => (f r -> r) -> Cata f i r
mkCata' = mkCata . const
{-# INLINE mkCata' #-}

-- | Creates an instance of @Cata@ from a parametrized catamorphism.
mkCata :: (Functor f) => (i -> f r -> r) -> Cata f i r
mkCata f = Cata f (const id)
{-# INLINE mkCata #-}

{-
-- | Creates an instance of @Cata@ that uses an additional argument propagated
-- down the stucture.
mkCata1 :: (Functor f) => (a -> f r -> r) -> Cata f (a -> r)
mkCata1 f = mkCata $ \r x -> f x (fmap (\f -> f x) r)

-- | Creates an instance of @Cata@ that uses additional arguments propagated
-- down the stucture.
mkCata2 :: (Functor f) => (a -> b -> f r -> r) -> Cata f (a -> b -> r)
mkCata2 f = mkCata $ \r x y -> f x y (fmap (\f -> f x y) r)

-- | Creates an instance of @Cata@ that uses additional arguments propagated
-- down the stucture.
mkCata3 :: (Functor f) => (a -> b -> c -> f r -> r) -> Cata f (a -> b -> c -> r)
mkCata3 f = mkCata $ \r x y z -> f x y z (fmap (\f -> f x y z) r)

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
-}
