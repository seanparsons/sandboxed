module Main where

import Control.Monad.Free.TH
import Control.Monad.Free

main :: IO ()
main = putStrLn "Hello World!"

cleanDown :: IO ()
cleanDown = do
  -- Read ~/.cabal/config so that we can restore it.
  -- Bulldoze

