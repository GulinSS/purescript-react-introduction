module App where

import Prelude

import React.Basic.Hooks as Hooks
import React.Basic.Hooks (Component, component, (/\))
import RouterContext (mkRouterContext)
import Router (mkRouter)

mkApp :: Component Unit
mkApp = do
  routerContextComponent /\ routerContext <- mkRouterContext
  router <- mkRouter routerContext
  component "App" \props -> Hooks.do
    pure $ routerContextComponent [
      router unit
     ]
