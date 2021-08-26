module AppRoute where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Foldable (oneOf)
import Data.Int (toStringAs, decimal)
import Routing.Match (Match)
import Routing.Match as Match
import Routing (match)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (pathname)
import Routing.PushState (PushStateInterface)
import Foreign (unsafeToForeign)
import Data.Either (fromRight)

data AppRoute
  = Home
  | Random Int Int

derive instance genericRoute :: Generic AppRoute _
derive instance eqRoute :: Eq AppRoute
derive instance ordRoute :: Ord AppRoute

instance showRoute :: Show AppRoute where
  show = genericShow

appRoute :: Match (Maybe AppRoute)
appRoute =
  oneOf
    [ Just <$> postRoute
    , Just <$> (Match.root *> pure Home <* Match.end)
    , pure Nothing
    ]
  where
    postRoute =
      Match.root *> Match.lit "pages" *> oneOf
        [ Random <$> (Match.lit "random" *> Match.int) <*> Match.int
        ] <* Match.end

getPathname :: AppRoute -> String
getPathname Home = "/"
getPathname (Random from to) = "/pages/random/" <> (toStringAs decimal from) <> "/" <> (toStringAs decimal to)

defaultAppRoute :: AppRoute
defaultAppRoute = Home

getAppRoute :: String -> AppRoute
getAppRoute pathname = fromMaybe defaultAppRoute
  $ fromRight Nothing
  $ match appRoute pathname

getInitialRoute :: Effect AppRoute
getInitialRoute = getAppRoute <$> (window >>= location >>= pathname)

class Monad m <= Navigate m where
  navigate :: AppRoute -> PushStateInterface -> m Unit
  begin :: PushStateInterface -> m Unit

instance navigateEffect :: Navigate Effect where
  navigate r nav = do
    nav.pushState (unsafeToForeign unit) (getPathname r)
  begin nav = do
    initialRoute <- getInitialRoute
    nav.replaceState (unsafeToForeign unit) (getPathname initialRoute)
