module Main where

import Prelude ( class Eq, class Show, Unit
               , bind, const, discard, map, mod, otherwise, pure, show, unit, unless, void, when, whenM
               , ($), (<<<), (>>>), (==), (<>), (+), (-), (<$>), (>>=), (=<<))
import Affjax (URL)
import Affjax (get) as AJAX
import Affjax.ResponseFormat ( ResponseFormatError
                             , arrayBuffer, printResponseFormatError )
import Audio.WebAudio.AudioBufferSourceNode ( defaultStartOptions, onended, setBuffer
                                            , startBufferSource, stopBufferSource )
import Audio.WebAudio.BaseAudioContext ( createBufferSource, createPanner
                                       , currentTime
                                       , destination, listener, newAudioContext
                                       , decodeAudioDataAsync )
import Audio.WebAudio.AudioListener (setOrientation) as Listener
import Audio.WebAudio.PannerNode ( DistanceModelType(..), PanningModelType(..)
                                 , setConeInnerAngle, setConeOuterAngle
                                 , setDistanceModel, setPanningModel
                                 , setMaxDistance )
import Audio.WebAudio.PannerNode (setOrientation) as Panner
import Audio.WebAudio.Types (AudioContext, AudioBuffer, AudioBufferSourceNode, PannerNode, connect)
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT, lift, runExceptT, except, mapExceptT, withExceptT)
import Control.Parallel (parTraverse)
import Data.Array (length) as Array
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Ref (Ref)
import Effect.Ref (modify, modify', modify_, new, read) as Ref
import Effect.Timer (setInterval, setTimeout)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (type_) as Event
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, ctrlKey, shiftKey, repeat, code)
import Web.UIEvent.KeyboardEvent (fromEvent, toEvent) as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

import Level
import Level (grab) as Level
import Sounds

moveTime :: GroundType -> Speed -> Int
moveTime Water Slow = 1000
moveTime Water Fast = 700
moveTime _ Slow = 500
moveTime _ Fast = 300

data Direction = Leftward | Rightward | Upward | Downward

type Game = {
  state :: { step :: Int
           , lastMoveTime :: Number
           , position :: Point2D
	   , canMove :: Boolean }
, buffer :: Map String AudioBuffer
, context :: AudioContext
, level :: Level
}

newGame :: AudioContext -> Map String AudioBuffer -> Level -> Effect (Ref Game)
newGame context buffer level = do
  Ref.new { state: { step: 0
                   , lastMoveTime: 0.0
                   , position: (unwrap level).start
                   , canMove: false }
          , buffer, context, level }

playMove :: GroundType -> Speed -> Ref Game -> Effect Unit
playMove ground speed ref = do
  n <- Ref.modify' (\g -> { state: g { state { step = (g.state.step + 1) `mod` 4 } }
                             , value: g.state.step + 1 }) ref
  game <- Ref.read ref
  case Map.lookup (moveSound ground speed n) game.buffer of
    Just buffer -> do
      source <- createBufferSource game.context
      connect source =<< destination game.context
      setBuffer buffer source
      void $ setTimeout (moveTime ground speed) $
        Ref.modify_ (_ { state { canMove = true} }) ref
      Ref.modify_ (_ { state { canMove = false } }) ref
      startBufferSource defaultStartOptions source
    _ -> pure unit

type Object = {
  type_ :: ObjectType
, panner :: PannerNode
, source :: AudioBufferSourceNode
}

mkObject :: AudioContext -> ObjectType -> Effect Object
mkObject ctx t = do
  panner <- createPanner ctx
  source <- createBufferSource ctx
  connect source panner
  connect panner =<< destination ctx
  pure { type_: t, panner: panner, source: source }
                     
data ObjectType = Zombie

app :: ExceptT String Aff Unit
app = do
  audio <- liftAff <<< liftEffect $ newAudioContext
  intro <- withExceptT printResponseFormatError <<< except =<<
           liftAff (loadSound "sounds/Game_Intro.wav" audio)
  liftAff <<< liftEffect $ play audio intro
  buffers <- withExceptT printResponseFormatError <<< except =<<
             liftAff (loadSounds audio sounds)
  level <- Level.grab "level0.json"
  liftAff <<< liftEffect $ log $
                           "Level with " <>
                           show (Array.length (unwrap level).rows) <>
                           " lines."
  game <- liftAff <<< liftEffect $ do
    g <- newGame audio buffers level
    addKeyboardListener g
    pure g
  mapExceptT liftEffect $ play' "Get_Gem" game
  liftAff <<< liftEffect $ Ref.modify_ (_ { state { canMove = true } }) game
  pure unit

main :: Effect Unit
main = launchAff_ $ runExceptT app >>= either (liftEffect <<< error) pure

addKeyboardListener :: Ref Game -> Effect Unit
addKeyboardListener ref = do
  target <- toEventTarget <$> window
  keydownListener <- eventListener $ update ref <<< KeyboardEvent.fromEvent
  addEventListener keydown keydownListener false target
  void $ setInterval 100 $ update ref Nothing

play :: AudioContext -> AudioBuffer -> Effect Unit
play ctx buf = do
  src <- createBufferSource ctx
  connect src =<< destination ctx
  setBuffer buf src
  startBufferSource defaultStartOptions src

play' :: String -> Ref Game
      -> ExceptT String Effect Unit
play' sound ref = do
  game <- liftEffect $ Ref.read ref
  case Map.lookup sound game.buffer of
    Just buffer -> lift do
      source <- createBufferSource game.context
      setBuffer buffer source
      connect source =<< destination game.context
      startBufferSource defaultStartOptions source
    Nothing -> throwError $ "Sound " <> sound <> " not known"

play'' :: String -> Effect Unit -> Ref Game
       -> Effect Unit
play'' sound next ref = do
  game <- Ref.read ref
  case Map.lookup sound game.buffer of
    Just buffer -> do
      source <- createBufferSource game.context
      setBuffer buffer source
      connect source =<< destination game.context
      onended source $ const next
      startBufferSource defaultStartOptions source
    Nothing -> error $ "Sound " <> sound <> " not known"

update :: Ref Game -> Maybe KeyboardEvent -> Effect Unit
update r = case _ of
  Just ke | Event.type_ (KeyboardEvent.toEvent ke) == keydown -> do
    log $ "keydown: " <> code ke
    unless (repeat ke) do
      case code ke, shiftKey ke, ctrlKey ke of
        "ArrowLeft",  true,  false  -> move Leftward Fast
        "ArrowLeft",  false, true   -> jump Leftward
        "ArrowLeft",  false, false  -> move Leftward Slow
        "ArrowRight", true,  false  -> move Rightward Fast
        "ArrowRight", false, true   -> jump Rightward
        "ArrowRight", false, false  -> move Rightward Slow
        "Space",      false, false  -> attack
        _, _, _ -> pure unit
  Nothing -> pure unit
  _ -> pure unit
 where
  attack = do
    log "Attacking!"
    play'' "Player_Miss" (pure unit) r
  jump dir = do
    play'' "Player_Jump" (play'' "Land_Stone" (pure unit) r) r
  move dir speed = do
    game <- Ref.read r
    when game.state.canMove
      if notBlocked dir game then case movingOn dir game of
        Just ground -> do
          Ref.modify_ (\game -> game { state { position = newPos dir game.state.position } }) r
          playMove ground speed r
        Nothing -> gameOver r
      else play'' "Angela_Blocked" (pure unit) r

gameOver ref = do
  Ref.modify_ (_ { state { canMove = false } }) ref
  play'' "Death_Standard" (pure unit) ref

newPos :: Direction -> Point2D -> Point2D
newPos Leftward { x, y } = { x: x - 1, y }
newPos Rightward { x, y } = { x: x + 1, y }
newPos Upward { x, y } = { x, y: y - 1 }
newPos Downward { x, y } = { x, y : y + 1 }

standingOn pos game = case at (newPos Downward pos) game.level of
  Floor floor -> Just floor
  _ -> Nothing

movingOn dir game = standingOn (newPos dir game.state.position) game

notBlocked dir game = case at (newPos dir game.state.position) game.level of
  Empty -> true
  otherwise -> false

movementEnded :: Ref Game -> Effect Unit
movementEnded _ = log $ "End of move"
