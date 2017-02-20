{-# LANGUAGE DeriveGeneric #-}
module ElmPackageInfo (Project(..), info) where


import Data.Aeson as Aeson
import Data.Maybe as Maybe
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified System.Exit as Exit


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


info :: FilePath -> IO Project
info root = do
  elmPackage <- BL.readFile (root </> "elm-package.json")
  let Just (ElmPackage version' repository') = Aeson.decode elmPackage
  case extractProject repository' of
    Right (user, repo) -> return $ Project version' user repo
    Left _ -> Exit.die "Not a valid repository"


extractProject :: T.Text -> Either P.ParseError (T.Text, T.Text)
extractProject str =
  P.parse projectParser "Not a valid repository" str


projectParser :: P.Parsec T.Text u0 (T.Text, T.Text)
projectParser = do
  _ <- P.string "https://github.com/"
  user <- P.manyTill P.anyChar (P.char '/')
  repo <- P.manyTill P.anyChar (P.string ".git")
  return $ (T.pack user, T.pack repo)
