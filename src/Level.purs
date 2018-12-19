module Level where

import Affjax (URL)
import Affjax (get) as AJAX
import Affjax.ResponseFormat (json, printResponseFormatError)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Array ((!!))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (charAt)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Prelude (bind, pure, ($), (<<<))
import Types

newtype Level = Level {
  rows :: Array String,
  start :: Point2D,
  updateInterval :: Int
}

defaultLevel :: Level
defaultLevel = Level {
  rows: [ "|   |"
        , "-----"]
, start: { x: 2, y: 0 }
, updateInterval: 100
}

derive instance newtypeLevel :: Newtype Level _
instance decodeJsonLevel :: DecodeJson Level where
  decodeJson json = do
    obj <- decodeJson json
    rows <- obj .: "rows"
    start <- obj .: "start"
    updateInterval <- obj .: "updateInterval"
    pure $ Level { rows, start, updateInterval }

grab :: URL -> ExceptT String Aff Level
grab url = do
  response <- liftAff $ AJAX.get json url
  body <- withExceptT printResponseFormatError <<< except $ response.body
  except <<< decodeJson $ body

data CellType = Empty | Floor GroundType | Wall | Door | Outside

at :: Point2D -> Level -> CellType
at pt (Level level)
  | Just r <- level.rows !! pt.y
  , Just c <- charAt pt.x r
  = case c of
    ' ' -> Empty
    '-' -> Floor Stone
    '_' -> Floor Sand
    '=' -> Floor Metal
    '~' -> Floor Water
    '#' -> Floor Staircase
    '|' -> Wall
    _   -> Empty
at _ _   = Outside
