module Main where

import qualified KnucleotideOld as Old
import qualified KnucleotideNew as New
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if args == ["--old"] then Old.main else New.main