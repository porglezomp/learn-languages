A zipper is a data structure for efficiently making local
modifications to and observations of a data structure.
A zipper gives you a "cursor" with which you can traverse
the data structure and make quick edits at the location of
the cursor.
The zipper of a list exposes a function to move the cursor
right and left, as well as making modifications.

\begin{code}
module Data.List.Zipper where
\end{code}

\begin{code}
data Zipper a = Zipper ![a] ![a]
\end{code}

We will want our Zipper to be an instance of Functor
so that we can map over it.
\begin{code}
instance Functor Zipper where
  fmap f (Zipper a b) = Zipper (map f a) (map f b)
\end{code}

We also need to be able to traverse the zipper, moving
the focus to either side. If it's not possible to move
the cursor any further, the zipper will be left alone.
\begin{code}
left :: Zipper a -> Zipper a
left (Zipper before (c:after)) = Zipper (c:before) after
left z = z

right :: Zipper a -> Zipper a
right (Zipper (n:before) after) = Zipper before (n:after)
right z = z
\end{code}

Beware, the cursor function isn't total. If the endp is true,
then the cursor has been moved off the end of the zipper, and
the function will fail. safeCursor is an alternative that's total.
\begin{code}
cursor :: Zipper a -> a
cursor (Zipper _ (c:_)) = c

safeCursor :: Zipper a -> Maybe a
safeCursor (Zipper _ (c:_)) = Just c
safeCursor _                = Nothing
\end{code}

\begin{code}
beginp :: Zipper a -> Bool
beginp (Zipper [] _) = True
beginp _             = False

endp :: Zipper a -> Bool
endp (Zipper _ []) = True
endp _             = False

emptyp :: Zipper a -> Bool
emptyp z = beginp z && endp z
\end{code}

\begin{code}
replace :: a -> Zipper a -> Zipper a
replace x (Zipper a (_:b)) = Zipper a (x:b)

insert :: a -> Zipper a -> Zipper a
insert x (Zipper a b) = Zipper a (x:b)

delete :: Zipper a -> Zipper a
delete (Zipper a (_:b)) = Zipper a b
delete z = z
\end{code}

\begin{code}
fromList :: [a] -> Zipper a
fromList = Zipper []
\end{code}
