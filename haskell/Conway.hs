{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import System.Environment

import Control.Comonad
import qualified Data.List.Zipper as L
import Data.Grid.Zipper

instance Comonad L.Zipper where
  coreturn = L.cursor
  cojoin z  = L.Zipper (tail $ iterate L.left z) (iterate L.right z)

instance Comonad Zipper where
  coreturn = cursor
  cojoin (Zipper z) = fmap Zipper $ Zipper $ roll $ roll z
    where roll a = L.Zipper
                   (tail $ iterate (fmap L.left) a)
                   (iterate (fmap L.right) a)

conway :: Zipper Bool -> Bool
conway z = if not self
           then neighbors == 3
           else neighbors > 1 && neighbors < 4
  where self = cursor z
        countTrue :: [Bool] -> Int
        countTrue = foldl (\ a b -> if b then a + 1 else a) 0
        neighbors = let c = L.cursor; l = L.left; r = L.right
                    in countTrue $ map ($ z)
                       [c.l.row.up,   c.row.up,   c.r.row.up
                       ,c.l.row,                  c.r.row
                       ,c.l.row.down, c.row.down, c.r.row.down
                       ]

surround :: Zipper a -> a -> Zipper a
surround (Zipper (L.Zipper a b)) i = Zipper $ L.Zipper
                                     (fmap surround' a ++ infinite)
                                     (fmap surround' b ++ infinite)
  where infinite = repeat $ L.Zipper (repeat i) (repeat i)
        surround' (L.Zipper x y) = L.Zipper (x ++ repeat i) (y ++ repeat i)

startState :: Zipper Bool
startState = surround center False
  where center = fromGrid [[False, True, False]
                          ,[False, False, True]
                          ,[True, True, True]
                          ]

printGrid :: Int -> Int -> Zipper Bool -> IO ()
printGrid w h grid = mapM_ printRow $ take h $ iterate down topmost
  where topmost = iterate up grid !! (h `div` 2)
        printRow r = do
          let leftmost = iterate left r !! (w `div` 2)
          mapM_ (\x -> if cursor x then putStr "#" else putStr " ") $
            take w $ iterate right leftmost
          putStrLn ""
          

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then putStrLn "Pass a number of steps to show"
    else do
    let count = read $ head args
    mapM_ (\x -> printGrid 10 10 x >> putStrLn "---") $
      take count $
      iterate (=>> conway) startState
