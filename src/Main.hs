{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
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


main :: IO ()
main = do
  Opts maybeRoot <- parseOpts
  cwd <- Dir.getCurrentDirectory
  let root = Maybe.maybe "." ((</>) cwd) maybeRoot
  project <- ElmPackageInfo.info root
  files <- findFiles $ buildArtefactsPath root project
  modules <- traverse InterfaceDecoder.decode files
  putStrLn $ InterfaceDecoder.toJson modules
  return ()


buildArtefactsPath :: FilePath -> Project -> FilePath
buildArtefactsPath root (Project version user repo) =
  root
  </> "elm-stuff" </> "build-artifacts" </> "0.18.0"
  </> T.unpack user </> T.unpack repo </> T.unpack version


findFiles :: FilePath -> IO [FilePath]
findFiles = find (excludedDirs filePath) (filePath ~~? "**" </> "*.elmi")


excludedDirs :: FindClause FilePath -> FindClause Bool
excludedDirs path = path /~? "**" </> "*.elmo"
