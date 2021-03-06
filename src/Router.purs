module Router where

import Prelude

import React.Basic.Hooks as Hooks
import RouterContext (ReactRouterContext)
import React.Basic.Hooks (Component, component)
import RouterContext (useRouterContext)
import React.Basic.DOM as R
import AppRoute (AppRoute(..))
import AppRoute (getPathname)
import PageRandom (mkPageRandom)
import Data.Tuple (Tuple(..))

mkRouter :: ReactRouterContext -> Component Unit
mkRouter rrc = do
  pageRandom <- mkPageRandom rrc
  component "Router" $ const $ Hooks.do
    { route } <- useRouterContext rrc
    pure $ case route of
      Home -> R.a { href: getPathname $ Random 0 20, children: [ R.text "Go Random" ] }
      Random from to -> pageRandom $ Tuple from to
