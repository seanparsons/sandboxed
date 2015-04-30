{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Traversable as T
import Control.Monad.Free.TH
import Control.Monad.Free
import System.Directory
import System.FilePath.Posix
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)

data Actions x = NoOp x
               | MkDir FilePath x
               | Delete FilePath x
               | ReadFile FilePath (Maybe B.ByteString -> x)
               | WriteFile FilePath B.ByteString x
               | InstallFromHackage [String] x
               deriving (Functor)

$(makeFree ''Actions)

printActions :: Free Actions x -> IO x  
printActions = iterM run
  where
    run (NoOp next) = next
    run (MkDir path next) = do
      putStr "Create directory "
      putStrLn path
      next
    run (Delete path next) = do
      putStr "Delete directory "
      putStrLn path
      next
    run (ReadFile path next) = do
      putStr "Capture the contents of "
      putStrLn path
      next $ Just B.empty
    run (WriteFile path _ next) = do
      putStr "Write out captured content to "
      putStrLn path
      next
    run (InstallFromHackage packages next) = do
      let packagesListing = L.intercalate ", " packages
      putStr "Install "
      putStrLn packagesListing
      next

preserve :: FilePath -> Free Actions x -> Free Actions x
preserve toPreserve innerAction = do
  possibleFileContents <- readFile toPreserve
  result <- innerAction
  T.traverse (writeFile toPreserve) possibleFileContents
  return result

main :: IO ()
main = do
  cleanDownActions <- cleanDown
  printActions cleanDownActions

cleanDown :: IO (Free Actions ())
cleanDown = do
  homeDirectory <- getHomeDirectory
  let dotCabalDir = combine homeDirectory ".cabal"
  let cabalConfigFile = combine dotCabalDir "config"
  let dotGHCDir = combine homeDirectory ".ghc"
  dotCabalExists <- doesDirectoryExist dotCabalDir
  cabalConfigExists <- doesFileExist cabalConfigFile
  dotGHCExists <- doesDirectoryExist dotGHCDir
  return $ do
    -- Delete ~/.cabal if it exists.
    let handleDotCabal = if dotCabalExists then delete dotCabalDir >> mkDir dotCabalDir else noOp
    -- Read ~/.cabal/config so that we can restore it.
    -- Bulldoze ~/.cabal.
    if cabalConfigExists then preserve cabalConfigFile handleDotCabal else handleDotCabal
    -- Bulldoze ~/.ghc.