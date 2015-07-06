{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Text.Printf
import System.Environment
import Data.Bits

import Control.Comonad
import Data.List.Zipper

instance Comonad Zipper where
  coreturn = cursor
  cojoin z = Zipper (tail $ iterate left z) (iterate right z)

rule :: Int -> Zipper Bool -> Bool
rule n (Zipper (l:_) (c:r:_)) = n `testBit` mask
  where mask = (    if l then 4 else 0)
               .|. (if c then 2 else 0)
               .|. (if r then 1 else 0)

printWindow :: Int -> Zipper Bool -> IO ()
printWindow n z = mapM_ (\x -> putStr $ if cursor x then "#" else " ") $
                  take n $
                  iterate right $
                  iterate left z !! (n `div` 2)

initialState :: Zipper Bool
initialState = Zipper (repeat False) (True : repeat False)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if length args < 1 then printf "usage: %s <rule number>\n" (name :: String)
    else case reads $ head args of
    [(r, "")] -> mapM_ (\x -> printWindow 20 x >> putStrLn "") $
                 take 10 $
                 iterate (=>> rule r) initialState
    _         -> printf "expected a number, got '%s'\n" (head args :: String)
