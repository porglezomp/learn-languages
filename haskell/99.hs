-- Problem 1
-- Find the last element of a list.

last' [] = error "Empty list has no last element."
last' (x:[]) = x
last' (_:xs) = last' xs

-- Problem 2
-- Find the last but one element of a list.

butLast' [] = error "Empty list has has no but last element."
butLast' (_:[]) = error "List with one element has no but last element."
butLast' (x:_:[]) = x
butLast' (_:xs) = butLast' xs

-- Problem 3
-- Find the kth element of a list

[] !!! _ = error "Out of bounds error"
_ !!! n | n <= 0 = error "Index must be 1 or greater"
(x:xs) !!! 1 = x
(x:xs) !!! n = xs !!! (n-1)

-- Problem 4
-- Find the number of elements in a list

length' [] = 0
length' (x:xs) = 1 + (length' xs)

-- Problem 5
-- Reverse a list

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome

isPalindrome' l = l == (reverse' l)

-- Problem 7
-- Flatten a nested list structure

data NestedList a = Elem a | List [NestedList a]

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.

compress [] = []
compress (x:[]) = [x]
compress (x:y:[])
    | x == y = [x]
    | otherwise = [x, y]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = x:(compress (y:xs))

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed into seperate
-- sublists.

pack [] = [[]]
pack (x:[]) = [[x]]
pack (x:xs) = let p@(px:pxs) = pack xs
              in
                if (x == (head px)) then
                    (x:px):pxs
                else
                    [x]:p

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number
-- of duplicates of the element E.

encode [] = []
encode list = [(length' sub, head sub) | sub <- pack list]