{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store where

import           Boris.Core.Data
import qualified Boris.Store.Schema as Schema

import           Mismi (AWS)

import qualified Test.Spine.Schema as Spine

-- NOTE: Don't change this, and don't make it random unless you
--       want me to take it out of your pay.....
environment :: Environment
environment =
  Environment "local"

withClean :: Environment -> AWS () -> AWS a -> AWS a
withClean e clean f =
  Spine.withClean (Schema.schema e) clean f
