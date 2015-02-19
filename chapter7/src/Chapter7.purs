module Chapter7 where

import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Apply

foreign import unsafeTracing "function unsafeTracing(x) { console.log(x); return x; }" :: forall a. a -> a

plusMaybe :: Maybe Number -> Maybe Number -> Maybe Number
plusMaybe = lift2 (+)
-- ditto for - / *

combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
    show = go ""
      where
          go :: String -> Tree a -> String
          go indent Leaf = indent ++ "Leaf"
          go indent (Branch left value right) =
              indent ++ "Value: " ++ show value ++ "\n" ++ go (indent ++ "  ") left ++ "\n" ++ go (indent ++ "  ") right

instance functorTree :: Functor Tree where
    (<$>) f Leaf = Leaf
    (<$>) f (Branch left value right) = Branch (f <$> left) (f value) (f <$> right)

instance foldableTree :: Foldable Tree where
    foldr f acc Leaf = acc
    foldr f acc (Branch left value right) = foldr f (f value (foldr f acc right)) left

    foldl f acc Leaf = acc
    foldl f acc (Branch left value right) = foldl f (f (foldl f acc left) value) right

    foldMap f Leaf = mempty
    foldMap f (Branch left value right) = foldMap f left <> f value <> foldMap f right

instance traverseTree :: Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Branch left value right) = Branch <$> traverse f left <*> f value <*> traverse f right

    sequence Leaf = pure Leaf
    sequence (Branch left value right) = Branch <$> sequence left <*> value <*> sequence right

    -- sequence in terms of traverse:
    -- sequence tree = traverse (\x -> x) tree

    -- traverse in terms of sequence:
    -- traverse f tree = sequence (f <$> tree)
