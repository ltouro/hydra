{-# LANGUAGE LambdaCase #-}

-- | Provide TH splice to retrieve a git revision string.
--
-- This module is a specialised version of
-- [gitDescribe](https://github.com/acfoltzer/gitrev/blob/master/src/Development/GitRev.hs#L150)
-- that returns either the revision from git if it exists, the content of an
-- environment variable `gitrev`, or @UNKNOWN@.
module Hydra.Version.Revision where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Version (showVersion)
import qualified Development.GitRev as GitRev
import Language.Haskell.TH (Exp (..), ExpQ, Lit (StringL), Q, runIO, stringE)
import Paths_hydra_prelude (version)
import System.Environment (lookupEnv)

gitDescribe :: ExpQ
gitDescribe = do
  gitRev <- GitRev.gitDescribe
  case gitRev of
    (LitE (StringL "UNKNOWN")) -> lookupGitRevInEnv >>= stringE
    other -> pure other

lookupGitRevInEnv :: Q String
lookupGitRevInEnv =
  runIO (lookupEnv "gitrev") >>= \case
    Nothing -> pure "UNKNOWN"
    Just r -> pure $ showVersion version <> "-" <> r
