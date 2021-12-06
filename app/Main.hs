module Main where

import Data.Text.Hematria

main :: IO ()
main = applyCommand =<< parseOptions 
