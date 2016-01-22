{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Boris.Store where

import           Boris.Core.Data

-- NOTE: Don't change this, and don't make it random unless you
--       want me to take it out of your pay.....
environment :: Environment
environment =
  Environment "testing"
