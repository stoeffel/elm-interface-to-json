{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson as Aeson
import Data.Maybe as Maybe
import Data.Maybe()
import Debug.Trace as Debug
import ElmPackageInfo (Project(..), info)
import Errors as E
import GHC.Generics (Generic)
import OptionParser (Opts(..), parseOpts)
import System.FilePath ((</>))
import System.FilePath.Find
import qualified Control.Monad (fmap)
import qualified Data.Text as T
import qualified InterfaceDecoder
import qualified System.Directory as Dir
import qualified System.Exit as Exit


main :: IO ()
main = do
  e <- runExceptT $ do
    Opts maybeRoot <- lift parseOpts
    cwd <- lift Dir.getCurrentDirectory
    let root = Maybe.maybe "." ((</>) cwd) maybeRoot
    project <- ElmPackageInfo.info root
    files <- lift $ findFiles $ buildArtefactsPath root project
    modules <- lift $ traverse InterfaceDecoder.decode files
    return $ InterfaceDecoder.toJson modules
  case e of
    Left n -> Exit.die $
      T.unpack $ T.unlines
      [ "Error: in elm-interface-to-json\n"
      , T.unwords ["   ", E.description n, "\n"]
      , T.unwords ["Hint:", E.hint n]
      ]

    Right json -> putStrLn json


buildArtefactsPath :: FilePath -> Project -> FilePath
buildArtefactsPath root (Project version user repo) =
  root
  </> "elm-stuff" </> "build-artifacts" </> "0.18.0"
  </> T.unpack user </> T.unpack repo </> T.unpack version


findFiles :: FilePath -> IO [FilePath]
findFiles = find (pure True) (extension ==? ".elmi")
