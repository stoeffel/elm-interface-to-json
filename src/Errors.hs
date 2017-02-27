{-# LANGUAGE OverloadedStrings #-}
module Errors (Error(..), description, hint) where


import qualified Data.Text as T
import System.FilePath


data Error
  = ElmPackageNotFound T.Text
  | ElmPackageInvalid

description :: Error -> T.Text
description (ElmPackageNotFound path) = T.unwords ["Couldn't find", path, "."]
description ElmPackageInvalid = "Your elm-package.json is invalid."


hint :: Error -> T.Text
hint (ElmPackageNotFound _) = "You can pass the path to your elm-package.json using `--path` (i.e. --path ./tests/)."
hint ElmPackageInvalid = "Check for typos in your elm-package.json"
