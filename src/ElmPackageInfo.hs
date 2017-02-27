{-# LANGUAGE DeriveGeneric #-}
module ElmPackageInfo (Project(..), info) where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson as Aeson
import Data.Maybe as Maybe
import Errors (Error(..))
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Text.Parsec as P


data Project = Project
  { version_ :: T.Text
  , user_ :: T.Text
  , repo_ :: T.Text
  } deriving (Show)


data ElmPackage = ElmPackage
  { version :: T.Text
  , repository :: T.Text
  } deriving (Generic, Show)


instance ToJSON ElmPackage
instance FromJSON ElmPackage


info :: FilePath -> EitherT Error IO Project
info root = do
  let elmPackagePath = (root </> "elm-package.json")
  elmPackageExists <- lift $ doesFileExist elmPackagePath
  _ <-
    case elmPackageExists of
      True -> lift $ return ()
      False -> left $ ElmPackageNotFound (T.pack elmPackagePath)
  elmPackage <- lift $ BL.readFile elmPackagePath
  ElmPackage version' repository' <-
    case Aeson.decode elmPackage of
      Nothing -> left ElmPackageInvalid
      Just j -> return j
  case extractProject repository' of
    Right (user, repo) -> return $ Project version' user repo
    Left _ -> left ElmPackageInvalid


extractProject :: T.Text -> Either P.ParseError (T.Text, T.Text)
extractProject str =
  P.parse projectParser "Not a valid repository" str


projectParser :: P.Parsec T.Text u0 (T.Text, T.Text)
projectParser = do
  _ <- P.string "https://github.com/"
  user <- P.manyTill P.anyChar (P.char '/')
  repo <- P.manyTill P.anyChar (P.string ".git")
  return $ (T.pack user, T.pack repo)
