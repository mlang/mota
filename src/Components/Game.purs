module Components.Game where

import Audio.WebAudio.AudioBufferSourceNode ( defaultStartOptions, onended, setBuffer
                                            , startBufferSource )
import Audio.WebAudio.BaseAudioContext (createBufferSource, destination)
import Audio.WebAudio.Types (AudioBuffer, AudioContext, connect)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect.Timer (clearInterval, setInterval, IntervalId)
import Prelude
import Prim.Row (class Union)
import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make, send)
import React.Basic.DOM (table_, td_, text, tr_)
import React.Basic.DOM.Components.GlobalEvents (defaultOptions, windowEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, ctrlKey, shiftKey, code)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

import Level (CellType(..), Level, at, movingOn, standingOn)
import Sounds (moveSound, landSound)
import React.Basic.StateUpdate ( class MonadStateUpdate, sideEffects
                               , action, delayedAction
                               , class MonadAsk, ask
                               , class MonadState
                               , get, gets, modify, modify_
                               , StateUpdateM, runStateUpdateM )
import Types (Direction(..), GroundType(..), Point2D, Speed(..), moveTime, shiftBy)

component :: Component Props
component = createComponent "Game"

type Props = { audioContext :: AudioContext
             , buffers :: Map String AudioBuffer
             , level :: Level }
data Action s = SetIntervalId IntervalId
              | KeyDown KeyboardEvent
              | Tick
              | Restart
              | Execute (StateUpdateM Props s (Action s) Unit)

type UpdateM s = StateUpdateM Props s (Action s)

game :: Props -> JSX
game props = make component
             { initialState, update, render, didMount, willUnmount } props where
  initialState = { intervalId: Nothing
                 , position: (unwrap props.level).start
                 , canMove: true, step: 0, direction: Rightward
                 , projectile: Nothing }
  didMount self = send self <<< SetIntervalId =<<
    setInterval (unwrap self.props.level).updateInterval (send self Tick)

  update self = runStateUpdateM self <<< case _ of
    Tick -> updateProjectile

    KeyDown ke -> whenM (gets _.canMove) case code ke, shiftKey ke, ctrlKey ke of
      "ArrowLeft",  true,  false -> move Leftward Fast
      "ArrowLeft",  false, true  -> jump Leftward
      "ArrowLeft",  false, false -> move Leftward Slow
      "ArrowRight", true,  false -> move Rightward Fast
      "ArrowRight", false, true  -> jump Rightward
      "ArrowRight", false, false -> move Rightward Slow
      "ArrowUp",    true,  false -> move Upward Fast
      "ArrowUp",    false, true  -> jump Upward
      "ArrowUp",    false, false -> move Upward Slow
      "ArrowDown",  true,  false -> move Downward Fast
      "ArrowDown",  false, true  -> jump Downward
      "ArrowDown",  false, false -> move Downward Slow
      "Space",      false, false -> attack
      "Enter",      false, false -> take
      _,            _,     _     -> pure unit

    SetIntervalId id -> modify_ (_ { intervalId = Just id })

    Restart -> do
      modify_ (_ { position = (unwrap props.level).start })
      thaw

    Execute more -> more

  render self
    = windowEvent { eventType: keydown
                  , handler: keydownHandler self
                  , options: defaultOptions } $
      plot self.props.level self.state.position { width: 15, height: 7 }

  willUnmount self = maybe (pure unit) clearInterval self.state.intervalId

  keydownHandler self = maybe (pure unit) (send self <<< KeyDown) <<< fromEvent

move
  :: forall state
   . Direction -> Speed
  -> UpdateM { canMove :: Boolean
             , direction :: Direction
             , position :: Point2D
             , step :: Int
             | state } Unit
move dir speed = ifM (not <$> looking dir) (turn dir) $
  canMoveTo dir >>= case _ of
    Right floor -> do
      modify_ (\old -> old { position = shiftBy dir 1 old.position })
      case floor of
        Just ground -> do
          n <- step
          play (moveSound ground speed n) (pure unit)
          freeze
          delayedExec (moveTime ground speed) thaw
        Nothing -> exec fall
    Left (Just reason) -> playFrozen ["Angela_" <> reason] (pure unit)
    Left Nothing -> pure unit

jump dir = pure unit

attack = playFrozen ["Player_Miss"] (pure unit)

take = pure unit

fall :: forall state
      . UpdateM { canMove :: Boolean, position :: Point2D | state } Unit
fall = do
  props <- ask
  state <- get
  let gravitate p n = let down = shiftBy Downward 1 p
                      in case at down props.level of
                           Floor _ -> n
                           _ -> gravitate down (n + 1)
  let height = gravitate state.position 0
  if height > 2
    then playFrozen ["Death_Fall"] $ action Restart
    else do
      state <- modify (_ { position = shiftBy Downward height state.position })
      case standingOn state.position props.level of
        Just ground -> playFrozen ["Player_Jump", landSound ground] $ pure unit
        Nothing -> action Restart

plot :: Level -> Point2D -> { width :: Int, height :: Int } -> JSX
plot level pos { width, height } =
  table_ $ row <$> enumFromTo leftTop.y (leftTop.y + height - 1)
 where
  row y = tr_ $ cell y <$> enumFromTo leftTop.x (leftTop.x + width - 1)
  cell y x | x == pos.x && y == pos.y = td_ [text "@"]
  cell y x = td_ [text case at { x, y } level of
    Empty       -> " "
    Floor Stone -> "-"
    Floor Sand  -> "_"
    Floor Water -> "~"
    Wall        -> "|"
    _           -> "?"
  ]
  leftTop = { x: pos.x - (width / 2), y: pos.y - (height / 2) }

updateProjectile
  :: forall p s
   . StateUpdateM p { projectile :: Maybe { n :: Int } | s } (Action { projectile :: Maybe { n :: Int } | s }) Unit
updateProjectile = maybe (pure unit) f =<< gets _.projectile where
  f { n } | n > 0 = do
            modify_ (_ { projectile = Just { n: n - 1 } })
            action Restart
          | otherwise = modify_ (_ { projectile = Nothing })

freeze
  :: forall state monad. MonadState { canMove :: Boolean | state } monad
  => monad Unit
freeze = modify_ (_ { canMove = false })

thaw
  :: forall state monad. MonadState { canMove :: Boolean | state } monad
  => monad Unit
thaw = modify_ (_ { canMove = true })

turn
  :: forall state monad. MonadState { direction :: Direction | state } monad
  => Direction
  -> monad Unit
turn dir = modify_ (_ { direction = dir })

playFrozen
  :: forall state
   . Array String
  -> UpdateM { canMove :: Boolean | state } Unit
  -> UpdateM { canMove :: Boolean | state } Unit
playFrozen xs more = freeze *> playAll xs (thaw *> more)

play
  :: forall state monad. MonadStateUpdate Props state (Action state) monad
  => String -> UpdateM state Unit -> monad Unit
play sound done = do
  props <- ask
  case lookup sound props.buffers of
    Just buffer -> sideEffects \self -> do
      source <- createBufferSource props.audioContext
      setBuffer buffer source
      connect source =<< destination props.audioContext
      onended source $ const (send self (Execute done))
      startBufferSource defaultStartOptions source
    Nothing -> exec done

playAll
  :: forall state monad. MonadStateUpdate Props state (Action state) monad
  => Array String -> UpdateM state Unit -> monad Unit
playAll xs done = uncons xs # maybe (exec done) \{ head, tail } ->
  play head (playAll tail done)

exec
  :: forall state monad. MonadStateUpdate Props state (Action state) monad
  => UpdateM state Unit -> monad Unit
exec = action <<< Execute

delayedExec
  :: forall state monad. MonadStateUpdate Props state (Action state) monad
  => Int -> UpdateM state Unit -> monad Unit
delayedExec ms = delayedAction ms <<< Execute

canMoveTo
  :: forall state monad
   . MonadAsk Props monad
  => MonadState { position :: Point2D | state } monad
  => Direction -> monad (Either (Maybe String) (Maybe GroundType))
canMoveTo dir = do
  props <- ask
  state <- get
  let here = state.position
  let there = shiftBy dir 1 here
  let check = case at there props.level of
                Floor Staircase -> Right (Just Staircase)
                Empty -> case at (shiftBy Downward 1 there) props.level of
                  Floor ground -> Right (Just ground)
                  _            -> Right Nothing
                Door -> Left (Just "Door")
                Wall -> Left (Just "Blocked")
                _    -> Left Nothing
  pure case at here props.level of
    Floor Staircase -> check
    Empty | dir /= Upward -> check
          | otherwise -> Left Nothing
    _ -> Left (Just "Trapped")

step
  :: forall state monad. MonadState { step :: Int | state } monad
  => monad Int
step = (_ + 1) <$> _.step <$> modify (\old -> old { step = (old.step + 1) `mod` 4 })

looking
  :: forall state monad. MonadState { direction :: Direction | state } monad
  => Direction -> monad Boolean
looking dir = (dir == _) <$> gets _.direction
