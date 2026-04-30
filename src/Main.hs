{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Assemblygen qualified
import Options.Generic
import Parse qualified
import System.Directory
import System.Exit
import System.FilePath
import System.IO
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

lex' :: Mixed -> Bool
lex' (Mixed labeled _) = not (parse labeled) && not (codegen labeled)

main :: IO ()
main = do
  record <- getRecord "Learning compilers and haskell"

  let inputFilePath = getFile record
  let baseFileName = takeBaseName inputFilePath
  let preprocessedFilePath = takeDirectory inputFilePath </> (baseFileName ++ ".i")
  let asmFilePath = takeDirectory inputFilePath </> (baseFileName ++ ".s")

  _ <- readProcess "gcc" ["-E", "-P", inputFilePath, "-o", preprocessedFilePath] ""

  readFile preprocessedFilePath >>= \source ->
    case Parse.parse source of
      Nothing -> error "Error when parsing"
      Just (rest, cAst) ->
        removePathForcibly preprocessedFilePath
          >> case lex' record of
            True -> pass
            False ->
              let
                (Mixed labeled _) = record
               in
                case codegen labeled of
                  False -> pass
                  True ->
                    print asmAst
                      >> writeFile asmFilePath (Assemblygen.emitAsm asmAst)
        where
          asmAst = Assemblygen.parseCAst cAst

-- TODO: pass to existing assembler
