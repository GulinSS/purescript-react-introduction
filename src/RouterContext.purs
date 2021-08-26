module RouterContext where

import Prelude

import AppRoute (AppRoute, begin, appRoute, getInitialRoute)
import Routing.PushState (PushStateInterface)
import React.Basic (ReactContext, JSX, createContext, provider)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic.Hooks (Hook, UseContext, useContext, component, useState, useEffectOnce, (/\))
import React.Basic.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)
import Effect (Effect)
import Routing.PushState (makeInterface, matches)
import Data.Tuple.Nested (type (/\))

type RouterContext = { route :: AppRoute, nav :: PushStateInterface }

type ReactRouterContext = ReactContext (Maybe RouterContext)

useRouterContext ::
  ReactRouterContext ->
  Hook (UseContext (Maybe RouterContext)) RouterContext
useRouterContext routerContext = Hooks.do
  maybeContextValue <- useContext routerContext
  pure case maybeContextValue of
    Nothing ->
      unsafeCrashWith
        "useContext can only be used in a descendant of \
        \the corresponding context provider component"
    Just contextValue -> contextValue

type RouterContextComponent props = Effect ((props -> JSX) /\ ReactRouterContext)

mkRouterContext :: RouterContextComponent (Array JSX)
mkRouterContext = do
  initialRoute <- getInitialRoute
  context <- createContext Nothing
  nav <- makeInterface
  c <- component "RouterContext" \children -> Hooks.do
    route /\ setRoute <- useState $ initialRoute
    useEffectOnce do
      nav # begin
      nav # matches appRoute \_ newRoute -> do
        setRoute $ flip fromMaybe newRoute
    pure
      $ provider context (Just { route: route, nav: nav }) children
  pure $ c /\ context
