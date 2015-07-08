module Main where

data Row a = Row [a] a [a]
type Col a = Row a
type Grid a = Col (Row a)

col :: [a] -> a -> [a] -> Col a
col = Row

left, right :: Row a -> Row a
left (Row (b:bs) x a)  = Row bs b (x:a)
right (Row b x (a:as)) = Row (x:b) a as
up, down :: Col a -> Col a
up = right
down = left
current :: Row a -> a
current (Row _ x _) = x

toRow :: [a] -> Row a
toRow (x:xs) = Row [] x xs

toGrid :: [[a]] -> Grid a
toGrid (x:xs) = col [] (toRow x) (map toRow xs)

wrap :: Grid Bool -> Grid Bool
wrap (Row b' x' a') = col (map wrapRow b' ++ repeat emptyRow)
                      (wrapRow x') (map wrapRow a' ++ repeat emptyRow)
  where emptyRow = Row (repeat False) False (repeat False)
        wrapRow (Row b x a) = Row (b ++ repeat False) x (a ++ repeat False)

initialConditions :: Grid Bool
initialConditions = wrap $ toGrid [[False, True,  True]
                                  ,[True,  True, False]
                                  ,[False, True, False]
                                  ]

nmap :: ((a, a, a, a, a, a, a, a, a) -> b) -> Grid a -> Grid b
nmap f x = col (map innermap' . tail $ iterate down x)
               (innermap' x)
               (map innermap' . tail $ iterate up x)
  where innermap' (Row (b:_) y (a:_)) = innermap b y a
        slide g a b c = tail $ zipWith3 ap
                        (iterate g a) (iterate g b) (iterate g c)
        innermap b y a = Row (slide left b y a) (ap b y a) (slide right b y a)
        ap (Row (b0:_) x0 (a0:_))
           (Row (b1:_) x1 (a1:_))
           (Row (b2:_) x2 (a2:_)) = f (b0, x0, a0, b1, x1, a1, b2, x2, a2)

rule :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Bool
rule (a, b, c,
      d, s, e,
      f, g, h) = if s
                 then pop > 1 && pop < 4
                 else pop == 3
  where pop = length . filter id $ [a, b, c, d, e, f, g, h]
                   

printGrid :: Int -> Int -> Grid Bool -> IO ()
printGrid w h grid = mapM_ (printLine . current)
                     (take h $ iterate up $ iterate down grid !! (h `div` 2))
                     >> putStrLn ('+' : replicate w '-' ++ "+")
  where printLine line = do
          putStr "|"
          mapM_ (\x -> putStr $ if current x then "#" else " ")
            $ take w $ iterate right $ iterate left line !! (w `div` 2)
          putStrLn "|"

main :: IO ()
main = do let w = 20
          putStrLn $ '+' : replicate w '-' ++ "+"
          mapM_ (printGrid w 20) $ take 500
            $ iterate (nmap rule) initialConditions
