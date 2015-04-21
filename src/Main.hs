{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Monad.Free.TH
import Control.Monad.Free
import System.Directory
import System.FilePath.Posix

data Actions x = NoOp
               | MkDir FilePath x
               | Delete FilePath x
               | ReadFile FilePath x
               | WriteFile FilePath String x
               
               | InstallFromHackage String x
               | End
               deriving (Functor)

$(makeFree ''Actions)

main :: IO ()
main = putStrLn "Hello World!"

cleanDown :: IO (Free Actions ())
cleanDown = do
  homeDirectory <- getHomeDirectory
  let dotCabalDir = combine homeDirectory ".cabal"
  let cabalConfigFile = combine dotCabalDir "config"
  let dotGHCDir = combine homeDirectory ".ghc"
  dotCabalExists <- doesFileExist dotCabalDir
  cabalConfigExists <- doesFileExist cabalConfigFile
  dotGHCExists <- doesFileExist dotGHCDir
  return $ do
    -- Need to write method that 
    let handleDotCabal = if dotCabalExists then delete dotCabalDir else noOp
    -- Read ~/.cabal/config so that we can restore it.
    -- Bulldoze ~/.cabal.
    if cabalConfigExists then preserve cabalConfigFile handleDotCabal else handleDotCabal
    -- Bulldoze ~/.ghc.