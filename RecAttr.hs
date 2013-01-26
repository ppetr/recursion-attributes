{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, TypeFamilies #-}
import Control.Applicative
import Control.Seq
import Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Functor.Foldable

import Control.Monad
import Data.Ratio
import Data.Maybe (listToMaybe)
import System.Environment

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

-- --------------------------------------------------------------------
-- Example: -----------------------------------------------------------
-- --------------------------------------------------------------------

-- As an example, we will compute the average value of a node in a tree.

-- Let's define a functor that makes a binary tree:
data BinTreeT a t = NodeT t a t | LeafT
  deriving (Show, Eq, Ord)
instance Functor (BinTreeT a) where
    fmap _ LeafT = LeafT
    fmap f (NodeT l x r) = NodeT (f l) x (f r)

-- And the binary tree itself:
data BinTree a = Node (BinTree a) a (BinTree a) | Leaf
  deriving (Show, Eq, Ord)
type instance Base (BinTree a) = BinTreeT a

-- Its (Un)foldable instances:
instance Foldable (BinTree a) where
    project Leaf            = LeafT
    project (Node l x r)    = NodeT l x r
instance Unfoldable (BinTree a) where
    embed LeafT           = Leaf
    embed (NodeT l x r)   = Node l x r

-- Computes the number of nodes in a tree.
count' :: (Num i) => CataBase (BinTree a) i
count' = mkCata l
  where
    l LeafT           = 0
    l (NodeT !l _ !r) = l + 1 + r

-- Sums all nodes in a tree.
sum' :: (Num n) => CataBase (BinTree n) n
sum' = mkCata l
  where
    l LeafT            = 0
    l (NodeT !l !n !r) = l + n + r

-- Computes the average value of a tree.
avg' :: (Fractional b) => CataBase (BinTree b) b
--avg' = (uncurry (/)) <$> (count' &> sum')
avg' = (/) <$> sum' <*> count'

-- We could even compute complex stuff like standard deviation in one pass, but
-- since we have to build a lazily evaluated function, unevaluated thunks of
-- the size of the tree are created anyway.
stddev2 :: (Fractional n) => CataBase (BinTree n) n
stddev2 = f <$> mkCata diff' <*> sum' <*> count'
  where
    f d s c = (d (s / c)) / (c - 1)
    diff' LeafT         _ = 0
    diff' (NodeT l x r) a = l a + (x - a)^2 + r a


-- Anamorphism that constructs Stern-Brocot tree of rational numbers of up to a
-- certain depth:
sternBrocot :: (Int, Rational, Rational) -> BinTreeT Rational (Int, Rational, Rational)
sternBrocot (0, _ , _ ) = LeafT
sternBrocot (n, mn, mx) = NodeT (n', mn, mid) mid (n', mid, mx)
  where
    mid = (numerator mn + numerator mx) % (denominator mn + denominator mx)
    n' = n - 1

main :: IO ()
main = do
    args <- getArgs
    let depth = maybe 10 read (listToMaybe args)
    let init = (depth, 0%1, 1%1)
    print $ runHylo avg' sternBrocot init
