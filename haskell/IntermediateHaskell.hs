module Misty where

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ []     = []
  furry f (x:xs) = f x : furry f xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just x) = Just (f x)
  furry _ Nothing  = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  -- furry :: (a -> b) -> (t -> a) -> (t -> b)
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  -- furry :: (a -> b) -> (EitherLeft t a) -> (EitherLeft t b)
  furry f (EitherLeft (Left x))  = EitherLeft . Left $ f x
  furry _ (EitherLeft (Right x)) = EitherLeft . Right $ x

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left x))  = EitherRight . Left $ x
  furry f (EitherRight (Right x)) = EitherRight . Right $ f x

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana _ []     = []
  banana f (x:xs) = f x ++ banana f xs
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing  = Nothing
  banana f (Just x) = f x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> t -> b) -> (t -> a) -> t -> b
  banana f x t = f (x t) t
  -- unicorn :: a -> t -> a
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  -- banana :: (a -> EitherLeft t b) -> EitherLeft t a -> EitherLeft t b
  banana f (EitherLeft (Left x))  = f x
  banana _ (EitherLeft (Right x)) = EitherLeft . Right $ x
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left x))  = EitherRight . Left $ x
  banana f (EitherRight (Right x)) = f x
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3

-- Thought process using a concrete example.
-- jellybean' :: Maybe (Maybe a) -> Maybe a
-- jellybean' Nothing = Nothing
-- jellybean' (Just x) = x
-- id :: a -> a, and here banana :: (a -> a) -> m a -> m a
-- jellybean' = banana id
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6

-- Thought process using a concrete example.
-- apple' :: Maybe a -> Maybe (a -> b) -> Maybe b
-- apple' _ Nothing = Nothing
-- apple' x (Just f) = banana f x
-- Alternatively,
-- apple' x f = g
--     where g = case f of
--          Nothing  -> Nothing
--          (Just h) -> banana (unicorn . h) x
-- Which, finally, reduces to:
-- apple' x = banana (\g -> banana (unicorn . g) x)
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x = banana $ \f -> banana (unicorn . f) x

-- Exercise 14
-- Relative Difficulty: 6

-- Thought process using a concrete example.
-- moppy' :: [a] -> (a -> Maybe b) -> Maybe [b]
-- moppy' (x:xs) f = case f x of
--   (Just y) -> case moppy' xs f of
--     (Just rest) -> Just $ y : rest
--     Nothing -> Nothing
--   Nothing  -> Nothing
-- Which is equivalent to,
-- moppy' (x:xs) f = case f x of
--   (Just y) -> banana (\ys -> unicorn $ y : ys) $ moppy' xs f
--   Nothing -> Nothing
-- Which finally reduces to,
-- moppy' (x:xs) f = banana innerlist $ f x
--   where innerlist y = banana (\ys -> unicorn $ y : ys) $ moppy' xs f
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy (x:xs) f = banana innerlist $ f x
  where innerlist y = banana (\ys -> unicorn $ y : ys) $ moppy xs f

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage l = moppy l id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: s -> (s, a)
  }

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
    furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
