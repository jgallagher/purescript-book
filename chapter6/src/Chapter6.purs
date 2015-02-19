module Chapter6 where

import Prelude hiding ((<#>))

import Data.Foldable
import Data.Monoid
import Data.Array

import Data.Hashable
import Control.MonadPlus

instance semigroupNumber :: Semigroup Number where
    (<>) = (+)

instance monoidNumber :: Monoid Number where
    mempty = 0

newtype Complex = Complex
    { real :: Number
    , imaginary :: Number
    }

instance showComplex :: Show Complex where
    show (Complex { real = real, imaginary = imaginary }) =
        (show real) ++ " + " ++ (show imaginary) ++ "i"

instance eqComplex :: Eq Complex where
    (==) (Complex { real = r1, imaginary = i1 }) (Complex { real = r2, imaginary = i2 }) =
        r1 == r2 && i1 == i2
    (/=) c1 c2 = not $ c1 == c2

data Nonempty a = Nonempty a [a]

instance semigroupNonempty :: Semigroup (Nonempty a) where
    (<>) (Nonempty a1 a1s) (Nonempty a2 a2s) = Nonempty a1 (a1s <> [a2] <> a2s)

instance showNonempty :: (Show a) => Show (Nonempty a) where
    show (Nonempty x xs) = "Nonempty: " ++ show (x : xs)

instance functorNonempty :: Functor Nonempty where
    (<$>) f (Nonempty x xs) = Nonempty (f x) (f <$> xs)

instance foldableNonempty :: Foldable Nonempty where
    foldr f acc (Nonempty x xs) = foldr f (f x acc) xs
    foldl f acc (Nonempty x xs) = foldl f (f acc x) xs
    foldMap f (Nonempty x xs) = f x <> foldMap f xs

instance eqNonempty :: (Eq a) => Eq (Nonempty a) where
    (==) (Nonempty x xs) (Nonempty y ys) = x == y && xs == ys
    (/=) n1 n2 = not $ n1 == n2

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
    (==) Infinite Infinite = true
    (==) Infinite (Finite _) = false
    (==) (Finite _) Infinite = false
    (==) (Finite x) (Finite y) = x == y
    (/=) e1 e2 = not $ e1 == e2

instance ordExtended :: (Ord a) => Ord (Extended a) where
    compare Infinite Infinite = EQ
    compare Infinite (Finite _) = GT
    compare (Finite _) Infinite = LT
    compare (Finite x) (Finite y) = compare x y

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
    foldr f acc (OneMore x xs) = foldr f (f x acc) xs
    foldl f acc (OneMore x xs) = foldl f (f acc x) xs
    foldMap f (OneMore x xs) = f x <> foldMap f xs

class (Monoid m) <= Action m a where
    act :: m -> a -> a

instance repeatAction :: Action Number String where
    act 0 _ = ""
    act n s = s ++ act (n - 1) s

instance elementwiseAction :: (Monoid m, Action m a) => Action m [a] where
    act m [] = []
    act m (x : xs) = [act m x] ++ act m xs

newtype Self m = Self m

instance showSelf :: (Show a) => Show (Self a) where
    show (Self x) = "Self: " ++ show x

instance selfAction :: (Monoid m) => Action m (Self m) where
    act m (Self x) = Self (m <> x)

class Unsafe

unsafeIndex :: forall a. (Unsafe) => [a] -> Number -> a
unsafeIndex xs i = Prelude.Unsafe.unsafeIndex xs i

unsafeLast :: forall a. (Unsafe) => [a] -> a
unsafeLast xs = unsafeIndex xs (length xs - 1)

hasDuplicates :: forall a. (Hashable a) => [a] -> Boolean
hasDuplicates xs = not $ null $ do
    i <- 0 .. length xs - 1
    let x = Prelude.Unsafe.unsafeIndex xs i
    y <- drop (i + 1) xs
    guard $ hashEqual x y
    guard $ x == y
    return x

newtype Uniform = Uniform Number

instance eqUniform :: Eq Uniform where
    (==) (Uniform u1) (Uniform u2) = u1 % 1.0 == u2 % 1.0
    (/=) (Uniform u1) (Uniform u2) = u1 % 1.0 /= u2 % 1.0

instance hashUniform :: Hashable Uniform where
    hash (Uniform u) = hash (u % 1.0)
