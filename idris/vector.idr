%default total

data Vect' : Nat -> Type -> Type where
  Nil  : Vect' Z a
  (::) : a -> Vect' k a -> Vect' (S k) a

data Fin' : Nat -> Type where
  FZ' : Fin' (S k)
  FS' : Fin' k -> Fin' (S k)

length : Vect' n a -> Nat
length Nil       = Z
length (_ :: xs) = S $ length xs

(++) : Vect' n a -> Vect' m a -> Vect' (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

infix 4 !!
(!!) : Vect' n a -> Fin' n -> a
(!!) (x :: _)  FZ'     = x
(!!) (x :: xs) (FS' k) = xs !! k
