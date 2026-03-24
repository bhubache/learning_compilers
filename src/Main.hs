{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Parse qualified
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data Labeled = Example {lex :: Bool, parse :: Bool, codegen :: Bool}
  deriving (Generic, Show)

instance ParseRecord Labeled

newtype Unlabeled = Unlabeled FilePath deriving (Generic, Show)

instance ParseRecord Unlabeled

data Mixed = Mixed Labeled Unlabeled deriving (Show)

instance ParseRecord Mixed where
  parseRecord = Mixed <$> parseRecord <*> parseRecord

-- TODO: Add -S option to emit assembly file but not assemble or link it

getFile :: Mixed -> FilePath
getFile (Mixed _ (Unlabeled filePath)) = filePath

main :: IO (String, Parse.Program)
main = do
  record <- getRecord "Learning compilers and haskell"

  let inputFilePath = getFile record
  let baseFileName = takeBaseName inputFilePath
  let preprocessedFilePath = takeDirectory inputFilePath </> (baseFileName ++ ".i")

  _ <- readProcess "gcc" ["-E", "-P", inputFilePath, "-o", preprocessedFilePath] ""

  ast <- Parse.parse preprocessedFilePath

  removePathForcibly preprocessedFilePath

  putStr (show ast)

  maybe exitFailure return ast

-- TODO: pass to existing assembler
