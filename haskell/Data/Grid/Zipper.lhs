The zipper for a grid (a list of lists) is implemented as
a zipper of list zippers. Technically speaking, this is not
strictly a zipper, since it has potentially higher costs as
the zipper gets larger, or as more motions have been made.

\begin{code}
module Data.Grid.Zipper where
import qualified Data.List.Zipper as L
\end{code}

\begin{code}
data Zipper a = Zipper (L.Zipper (L.Zipper a))
\end{code}

\begin{code}
instance Functor Zipper where
  fmap f (Zipper a) = Zipper (fmap (fmap f) a)
\end{code}

Up and down are essentially the same function as right and left
are for the zipper of a list. Left and right exploit Haskell's
laziness and map left and right over each individal row of the
grid.
\begin{code}
up :: Zipper a -> Zipper a
up (Zipper z) = Zipper $ L.right z

down :: Zipper a -> Zipper a
down (Zipper z) = Zipper $ L.left z

left :: Zipper a -> Zipper a
left (Zipper z) = Zipper $ fmap L.left z

right :: Zipper a -> Zipper a
right (Zipper z) = Zipper $ fmap L.right z
\end{code}

\begin{code}
row :: Zipper a -> L.Zipper a
row (Zipper (L.Zipper _ (c:_))) = c

cursor :: Zipper a -> a
cursor z = L.cursor $ row z
\end{code}

\begin{code}
topp :: Zipper a -> Bool
topp (Zipper z) = L.endp z

bottomp :: Zipper a -> Bool
bottomp (Zipper z) = L.beginp z

endp :: Zipper a -> Bool
endp z = L.endp $ row z

beginp :: Zipper a -> Bool
beginp z = L.beginp $ row z

emptyp :: Zipper a -> Bool
emptyp z = topp z && bottomp z
\end{code}

\begin{code}
replace :: a -> Zipper a -> Zipper a
replace x (Zipper (L.Zipper a (c:b))) = Zipper . L.Zipper a $ L.replace x c : b

deleteRow :: Zipper a -> Zipper a
deleteRow (Zipper z) = Zipper $ L.delete z

deleteCol :: Zipper a -> Zipper a
deleteCol (Zipper z) = Zipper $ fmap L.delete z
\end{code}

\begin{code}
fromGrid :: [[a]] -> Zipper a
fromGrid l = Zipper . L.Zipper [] $ map L.fromList l
\end{code}
