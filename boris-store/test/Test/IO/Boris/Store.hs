{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store where

import           Boris.Core.Data
import qualified Boris.Store.Lifecycle as SL

import           Mismi (AWS, awsBracket)

import           P

-- NOTE: Don't change this, and don't make it random unless you
--       want me to take it out of your pay.....
environment :: Environment
environment =
  Environment "testing"

withClean :: Environment -> AWS () -> AWS a -> AWS a
withClean e clean f =
  awsBracket (SL.initialise e) (const $ clean) (const $ clean >> f)
