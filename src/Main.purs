module Main where

import Prelude

import Effect (Effect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Web.DOM.NonElementParentNode (getElementById)
import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import App (mkApp)
import React.Basic.DOM (render)

main :: Effect Unit
main = do
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      app <- mkApp
      render (app unit) c
