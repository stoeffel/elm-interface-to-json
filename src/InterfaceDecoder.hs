{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module InterfaceDecoder (decode, toJson, Module(..)) where


import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Elm.Compiler.Module as Elm.Module
import qualified Elm.Compiler.Type as Elm.Type
import qualified System.FilePath as P
import System.FilePath.Find()


data Module = Module
  { moduleName :: T.Text
  , types :: [Function]
  } deriving (Generic, Show)


data Function = Function
  { name :: T.Text
  , signature :: T.Text
  } deriving (Generic, Show)


instance Aeson.ToJSON Module
instance Aeson.FromJSON Module
instance Aeson.ToJSON Function
instance Aeson.FromJSON Function


decode :: P.FilePath -> IO Module
decode path = do
  binary <- BL.readFile path
  return
    $ Module (pathToModuleName path)
    $ fmap toFunction
    $ Map.toList
    $ fmap Elm.Type.toString
    $ Map.delete "::"
    $ Elm.Module.interfaceAliasedTypes (B.decode binary)


toFunction :: (String, String) -> Function
toFunction (a, b) = Function (T.pack a) (T.pack b)


pathToModuleName :: FilePath -> T.Text
pathToModuleName =
  T.replace "-" "."
  . T.pack
  . P.takeFileName
  . P.dropExtension


toJson :: [Module] -> String
toJson = T.unpack . TE.decodeUtf8 . BSL.toStrict . Aeson.encode
