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
import Prelude (bind, pure, ($), (<<<), (/), (>>=))
import Sounds (GroundType(..))

newtype Level = Level {
  rows :: Array String,
  cellWidth :: Int,
  start :: Point2D
}

type Point2D = { x :: Int, y :: Int }

derive instance newtypeLevel :: Newtype Level _
instance decodeJsonLevel :: DecodeJson Level where
  decodeJson json = do
    obj <- decodeJson json
    rows <- obj .: "rows"
    cellWidth <- obj .: "cellWidth"
    start <- obj .: "start"
    pure $ Level { rows: rows, cellWidth: cellWidth, start: start }

grab :: URL -> ExceptT String Aff Level
grab url = do
  response <- liftAff $ AJAX.get json url
  body <- withExceptT printResponseFormatError <<< except $ response.body
  except <<< decodeJson $ body

data CellType = Empty | Floor GroundType | Wall | Door | Outside

at :: Point2D -> Level -> CellType
at pt (Level level)
  | Just r <- level.rows !! pt.y
  , Just c <- charAt (pt.x / level.cellWidth) r
  = case c of
    ' ' -> Empty
    '-' -> Floor Stone
    '~' -> Floor Water
    '#' -> Floor Staircase
    '|' -> Wall
    _   -> Empty
at _ _   = Outside
