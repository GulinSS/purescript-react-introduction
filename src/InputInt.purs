module InputInt where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState', useState, (/\))
import React.Basic.Hooks as Hooks
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Data.Int (toStringAs, fromStringAs, decimal)
import React.Basic.Events (handler, handler_)
import React.Basic.DOM.Events (targetValue)

mkInputInt :: String -> Component {
    value :: Maybe Int
  , onChange :: Int -> Effect Unit
  }
mkInputInt placeholder =
  component "InputInt" \props -> Hooks.do
    parsedValue /\ setParsedValue <- useState' props.value
    rawValue /\ setRawValue <- useState $ maybe "" (toStringAs decimal) props.value

    pure $ R.input {
        value: rawValue
      , type: "number"
      , placeholder: placeholder
      , onChange: handler targetValue \mStr -> do
          setRawValue \old ->
            case mStr of
              Nothing -> old
              Just v -> v
          setParsedValue $ mStr >>= fromStringAs decimal
      , onBlur: handler_ $ do
          case parsedValue of
            Nothing -> pure unit
            Just v -> props.onChange v
    }