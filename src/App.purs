module App where

import Prelude

import React.Basic.Hooks as Hooks
import React.Basic.Hooks (Component, component, fragment)
import React.Basic.DOM as R

mkApp :: Component Unit
mkApp = do
  component "App" $ const $ Hooks.do
    pure $ fragment [
      R.text "Hello world!"
    ]
