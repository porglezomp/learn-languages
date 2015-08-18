{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import System.IO (hFlush, stdout)

data Term = App Term Term
          | Var Char
          | Lam Char Term
          deriving Eq

instance Show Term where
  show (Lam name body@(Lam _ _)) = '\\' : name : tail (show body)
  show (Lam name body) = '\\' : name : '.' : show body
  show (App l r) = lhs l ++ rhs r
    where lhs (Lam _ _) = '(' : show l ++ ")"
          lhs _ = show l
          rhs (App _ _) = '(' : show r ++ ")"
          rhs (Lam _ _) = '(' : show r ++ ")"
          rhs _         = show r
  show (Var name) = [name]

parse "" = (Var '\0', "")
parse input =
  let (first, text) = parseTerminal input
  in parse' first text
  where
    parse' obj "" = (obj, "")
    parse' obj text@(')':_) = (obj, text)
    parse' obj text =
      let (next, unparsed) = parseTerminal text
      in parse' (App obj next) unparsed

parseTerminal ('(':text) =
  let (obj, ')':unparsed) = parse text
  in (obj, unparsed)
parseTerminal ('\\':name:'.':text) =
  let (body, unparsed) = parse text
  in (Lam name body, unparsed)
parseTerminal ('\\':namesAndRest) =
  let (names, '.':rest) = break (== '.') namesAndRest
      (body, unparsed) = parse rest
      items = foldr Lam body names
  in (items, unparsed)
parseTerminal (x:text) = (Var x, text)

isReducable (Var _) = False
isReducable (App (Var _) r) = isReducable r
isReducable (App (Lam _ _) _) = True
isReducable (App l@(App _ _) r) = isReducable l || isReducable r
isReducable (Lam _ body) = isReducable body

reduce (App (Lam name body) r) = replace body name r
reduce (App l@(App _ _) r) = App (reduce l) r
reduce (App l r) = App (reduce l) (reduce r)
reduce (Lam name body) = Lam name $ reduce body
reduce x = x

replace var@(Var v) from to
  | v == from = to
  | otherwise = var
replace lam@(Lam v body) from to
  | v == from = lam
  | otherwise = Lam v (replace body from to)
replace (App l r) from to = App (replace l from to) (replace r from to)
  
main = do
  putStrLn "Type lambda calculus expressions. ^C to exit."
  mainloop

takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else [x]
takeWhileDifferent' (x:y:xs)
  | x == y    = [x, y]
  | otherwise = x : takeWhileDifferent' (y:xs)
takeWhileDifferent' x = x

mainloop = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let expr = fst $ parse input
  mapM_ print $ takeWhileDifferent' . takeWhile' isReducable $ iterate reduce expr
  mainloop
