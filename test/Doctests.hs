module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)
import Prelude (IO, ($))

main :: IO ()
main = do
  sourceFiles <- glob "src/**/*.hs"
  doctest $
    "-XNoImplicitPrelude" :
    "-XLambdaCase" :
    sourceFiles