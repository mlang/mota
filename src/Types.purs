module Types where

import Data.Argonaut (class DecodeJson, caseJsonString)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Show, (+), (-))

type Point2D = { x :: Int, y :: Int }

data Direction = Leftward | Rightward | Upward | Downward
derive instance eqDirection :: Eq Direction
instance decodeJsonDirection :: DecodeJson Direction where
  decodeJson = caseJsonString (Left "Value is not a string") case _ of
    "Leftward"  -> Right Leftward
    "Rightward" -> Right Rightward
    "Upward"    -> Right Upward
    "Downward"  -> Right Downward
    _           -> Left "Value is not a direction"

shiftBy :: Direction -> Int -> Point2D -> Point2D
shiftBy Leftward n { x, y } = { x: x - n, y }
shiftBy Rightward n { x, y } = { x: x + n, y }
shiftBy Upward n { x, y } = { x, y: y - n }
shiftBy Downward n { x, y } = { x, y: y + n }

data GroundType = Bridge | Dirt | Ledge | Metal | Mud | Sand
                | Staircase | Stone | Water | Wood

derive instance genericGroundType :: Generic GroundType _
derive instance eqGroundType :: Eq GroundType
instance showGroundType :: Show GroundType where show = genericShow

data Speed = Slow | Fast

derive instance genericSpeed :: Generic Speed _
derive instance eqSpeed :: Eq Speed
instance showSpeed :: Show Speed where show = genericShow

moveTime :: GroundType -> Speed -> Int
moveTime Water Slow = 1000
moveTime Water Fast = 700
moveTime _ Slow = 350
moveTime _ Fast = 250

data Weapon = Dagger | Bow | Whip | Sword | Pistol | Magnum | Shotgun | Uzi

derive instance genericWeapon :: Generic Weapon _
instance showWeapon :: Show Weapon where show = genericShow
