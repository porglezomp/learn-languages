%default total

-- Let's prove all of the Applicative laws!
-- It's surpisingly easy, every single one of these could be handled by
-- pattern expansion and the automatic proof finder (all refl!)

class Functor' (f : Type -> Type) where
  fmap' : (a -> b) -> f a -> f b

infixl 4 |*|
class Functor' f => Applicative' (f : Type -> Type) where
  pure' : a -> f a
  (|*|) : f (a -> b) -> f a -> f b
  
data Maybe' a = Just' a | Nothing'
  
instance Functor' Maybe' where
  fmap' _ Nothing'  = Nothing'
  fmap' f (Just' x) = Just' $ f x
  
instance Applicative' Maybe' where
  pure' = Just'
  (|*|) (Just' f) (Just' x) = Just' $ f x
  (|*|) _         _         = Nothing'

infixl 4 |$|
(|$|) : Functor' f => (a -> b) -> f a -> f b
(|$|) = fmap'

functorFmap : (f : a -> b) -> (x : Maybe' a) -> fmap' f x = pure' f |*| x
functorFmap _ (Just' x) = Refl
functorFmap _ Nothing' = Refl

applicativeIdentity : (v : Maybe' a) -> pure' id |*| v = v
applicativeIdentity (Just' x) = Refl
applicativeIdentity Nothing' = Refl

applicativeComposition : (u : Maybe' (b -> c)) ->
                         (v : Maybe' (a -> b)) ->
                         (w : Maybe' a) ->
                         pure' (.) |*| u |*| v |*| w = u |*| (v |*| w)
applicativeComposition (Just' a) (Just' b) (Just' c) = Refl
applicativeComposition Nothing'  (Just' b) (Just' c) = Refl
applicativeComposition _         Nothing'  (Just' c) = Refl
applicativeComposition _         _         Nothing'  = Refl

applicativeHomomorphism : (f : a -> b) -> (x : a) -> (the ((a -> b) ->
                          Maybe' (a -> b)) pure') f |*| pure' x = pure' (f x)
applicativeHomomorphism f x = Refl

applicativeInterchange : (u : Maybe' (a -> b)) -> (x : a) ->
                         u |*| pure' x = pure' (\f => f x) |*| u
applicativeInterchange (Just' y) x = Refl
applicativeInterchange Nothing' x = Refl
