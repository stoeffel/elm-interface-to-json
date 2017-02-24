{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module OptionParser (parseOpts, Opts(..)) where


import Data.Maybe as Maybe
import Data.Monoid
import Options.Applicative hiding (str)
import Options.Applicative.Builder()
import Data.Text()


newtype Opts = Opts
  { packagePath :: Maybe FilePath
  }


parseOpts :: IO Opts
parseOpts = execParser opts


parser :: Parser Opts
parser =
  Opts
    <$> option (maybeReader pathReadM)
      (  long "path"
      <> value Nothing
      <> help "Default is whatever is the current working dir."
      )
  where
    pathReadM :: String -> Maybe (Maybe FilePath)
    pathReadM "" = Just Nothing
    pathReadM str = Just $ Just str


opts :: ParserInfo Opts
opts = info (helper <*> parser) fullDesc
