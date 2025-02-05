{-# LANGUAGE ScopedTypeVariables #-}
module BinaryOperationHelper
  ( isAssociative
  , isCommutative
  , hasIdentity
  , identityElement
  , isIdempotent
  , leftCancellation
  , rightCancellation
  , isDistributive
  , isAlternating
  , isFlexible
  ) where

-- | Test whether a binary operation is associative over a sample list.
-- That is, for all x, y, z: op x (op y z) == op (op x y) z.
isAssociative :: (Eq a) => (a -> a -> a) -> [a] -> Bool
isAssociative op xs =
  all (\(x, y, z) -> op x (op y z) == op (op x y) z) triples
  where
    triples = [(x, y, z) | x <- xs, y <- xs, z <- xs]

-- | Test whether a binary operation is commutative over a sample list.
-- That is, for all x, y: op x y == op y x.
isCommutative :: (Eq a) => (a -> a -> a) -> [a] -> Bool
isCommutative op xs =
  all (\(x, y) -> op x y == op y x) pairs
  where
    pairs = [(x, y) | x <- xs, y <- xs]

-- | Check if there exists an identity element among the sample values.
-- An identity element e satisfies: for all x, op e x == x && op x e == x.
hasIdentity :: (Eq a) => (a -> a -> a) -> [a] -> Bool
hasIdentity op xs = any (\e -> all (\x -> op e x == x && op x e == x) xs) xs

-- | Find an identity element from the sample values if one exists.
identityElement :: (Eq a) => (a -> a -> a) -> [a] -> Maybe a
identityElement op xs =
  case filter (\e -> all (\x -> op e x == x && op x e == x) xs) xs of
    (e:_) -> Just e
    []    -> Nothing

-- | Test for idempotence: for all x in xs, op x x == x.
isIdempotent :: (Eq a) => (a -> a -> a) -> [a] -> Bool
isIdempotent op xs = all (\x -> op x x == x) xs

-- | Check the left cancellation property:
-- For all x, y, z: if op x y == op x z then y == z.
leftCancellation :: (Eq a) => (a -> a -> a) -> [a] -> Bool
leftCancellation op xs =
  all (\(x, y, z) -> (op x y == op x z) `implies` (y == z)) triples
  where
    triples = [(x, y, z) | x <- xs, y <- xs, z <- xs]
    implies p q = not p || q

-- | Check the right cancellation property:
-- For all x, y, z: if op y x == op z x then y == z.
rightCancellation :: (Eq a) => (a -> a -> a) -> [a] -> Bool
rightCancellation op xs =
  all (\(x, y, z) -> (op y x == op z x) `implies` (y == z)) triples
  where
    triples = [(x, y, z) | x <- xs, y <- xs, z <- xs]
    implies p q = not p || q

-- | Test the distributive property.
-- Given two binary operations, op and op', check both left and right distributivity:
-- op x (op' y z) == op (op x y) (op x z)
-- and
-- op (op' y z) x == op (op y x) (op z x)
isDistributive :: (Eq a) => (a -> a -> a) -> (a -> a -> a) -> [a] -> Bool
isDistributive op op' xs =
  let triples = [(x, y, z) | x <- xs, y <- xs, z <- xs]
  in  all (\(x, y, z) -> op x (op' y z) == op (op x y) (op x z)) triples &&
      all (\(x, y, z) -> op (op' y z) x == op (op y x) (op z x)) triples

-- | Test alternativity (a property found in alternative algebras).
-- Check both left alternativity: op x (op x y) == op (op x x) y
-- and right alternativity: op (op y x) x == op y (op x x)
isAlternating :: (Eq a) => (a -> a -> a) -> [a] -> Bool
isAlternating op xs =
  let pairs = [(x, y) | x <- xs, y <- xs]
  in  all (\(x, y) -> op x (op x y) == op (op x x) y) pairs &&
      all (\(x, y) -> op (op y x) x == op y (op x x)) pairs

-- | Test the flexible law: for all x, y in xs, op x (op y x) == op (op x y) x.
isFlexible :: (Eq a) => (a -> a -> a) -> [a] -> Bool
isFlexible op xs =
  let pairs = [(x, y) | x <- xs, y <- xs]
  in  all (\(x, y) -> op x (op y x) == op (op x y) x) pairs
