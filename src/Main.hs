{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Control.Monad.Free
import Data.Aeson as Aeson
import Data.Maybe as Maybe
import Debug.Trace as Debug
import GHC.Generics (Generic)
import OptionParser (Opts(..), parseOpts)
import ElmPackageInfo (Project(..), info)
import System.FilePath ((</>))
import System.FilePath.Find
import qualified Control.Monad (fmap)
import Data.Maybe()
import qualified Data.Text as T
import qualified InterfaceDecoder
import qualified System.Directory as Dir


data ProgramF next
  = ParseOptions (FilePath -> next)
  | ReadElmPackage FilePath (Project -> next)
  | FindElmiFiles Project FilePath ([FilePath] -> next)
  | DecodeInterfaces [FilePath] ([InterfaceDecoder.Module] -> next)
  | OutputJson [InterfaceDecoder.Module] next
  deriving (Functor)

type Program = Free ProgramF

parseOptions :: Program FilePath
parseOptions = liftF $ ParseOptions id


readElmPackage :: FilePath -> Program Project
readElmPackage path = liftF $ ReadElmPackage path id


findElmiFiles :: Project -> FilePath -> Program [FilePath]
findElmiFiles project root = liftF $ FindElmiFiles project root id


decodeInterfaces :: [FilePath] -> Program [InterfaceDecoder.Module]
decodeInterfaces files = liftF $ DecodeInterfaces files id


outputJson :: [InterfaceDecoder.Module] -> Program ()
outputJson modules = liftF $ OutputJson modules ()

program :: Program ()
program = do
  root <- parseOptions
  project <- readElmPackage root
  files <- findElmiFiles project root
  modules <- decodeInterfaces files
  outputJson modules


ppInterpreter :: ProgramF next -> IO next
ppInterpreter (ParseOptions next) = putStrLn "parsing options" >> return (next $ ".")
ppInterpreter (ReadElmPackage root next) = putStrLn ("reading elm-package.json in " ++ root) >> return (next $ Project "0.0.0" "user" "repo")
ppInterpreter (FindElmiFiles _ _ next) = putStrLn "finding elmi files" >> return (next [])
ppInterpreter (DecodeInterfaces _ next) = putStrLn "decoding interfaces" >> return (next [])
ppInterpreter (OutputJson _ next) = putStrLn "printing json" >> return next


ioInterpreter :: ProgramF next -> IO next
ioInterpreter (ParseOptions next) = parseOptionsIO next
ioInterpreter (ReadElmPackage root next) = fmap next $ ElmPackageInfo.info root
ioInterpreter (FindElmiFiles project root next) = fmap next $ findFiles $ buildArtefactsPath project root
ioInterpreter (DecodeInterfaces files next) = fmap next $ traverse InterfaceDecoder.decode files
ioInterpreter (OutputJson modules next) = (putStrLn $ InterfaceDecoder.toJson modules) >> return next


main :: IO ()
main = foldFree ioInterpreter program


parseOptionsIO :: (FilePath -> next) -> IO next
parseOptionsIO next = do
  Opts maybeRoot <- parseOpts
  cwd <- Dir.getCurrentDirectory
  let root = Maybe.maybe cwd ((</>) cwd) maybeRoot
  return $ next root


buildArtefactsPath :: Project -> FilePath -> FilePath
buildArtefactsPath (Project version user repo) root =
  root
  </> "elm-stuff" </> "build-artifacts" </> "0.18.0"
  </> T.unpack user </> T.unpack repo </> T.unpack version


findFiles :: FilePath -> IO [FilePath]
findFiles = find (excludedDirs filePath) (filePath ~~? "**" </> "*.elmi")


excludedDirs :: FindClause FilePath -> FindClause Bool
excludedDirs path = path /~? "**" </> "*.elmo"
