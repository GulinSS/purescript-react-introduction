module PageRandom where

import Prelude

import AppRoute (AppRoute(..), navigate)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState', (/\), useEffectOnce, useMemo, useEffect)
import React.Basic.Hooks as Hooks
import Data.Tuple (Tuple, fst, snd)
import InputInt (mkInputInt)
import Data.Maybe (Maybe(..), maybe)
import Effect.Random (randomInt)
import Data.Int (toStringAs, decimal)
import RouterContext (ReactRouterContext, useRouterContext)

mkPageRandom :: ReactRouterContext -> Component (Tuple Int Int)
mkPageRandom rrc = do
  inputIntFrom <- mkInputInt "From"
  inputIntTo <- mkInputInt "To"
  component "PageRandom" $ \(from /\ to) -> Hooks.do
    { nav } <- useRouterContext rrc

    result /\ setResult <- useState' $ Nothing

    setFrom <- useMemo to \_ -> \v -> nav # (navigate $ Random v to)
    setTo <- useMemo from \_ -> \v -> nav # (navigate $ Random from v)

    useEffect [from, to] do
      rndInt <- randomInt from to
      setResult $ Just rndInt
      pure mempty

    pure $ Hooks.fragment [
      R.form { children: [
        R.div_ [ R.label_ [
          R.text "From: ",
          inputIntFrom {
            value: Just from
          , onChange: setFrom
          }
        ] ]
      , R.div_ [ R.label_ [
          R.text "To: ",
          inputIntTo {
            value: Just to
          , onChange: setTo
          }
        ] ]
      ] }
    , R.div_ [
        R.text ("Result: " <> maybe "" (toStringAs decimal) result)
      ]
    ]