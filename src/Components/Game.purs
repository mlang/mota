module Components.Game where

import Audio.WebAudio.AudioBufferSourceNode ( defaultStartOptions, onended, setBuffer
                                            , startBufferSource )
import Audio.WebAudio.BaseAudioContext (createBufferSource, destination)
import Audio.WebAudio.Types (AudioBuffer, AudioContext, connect)
import Data.Array (uncons)
import Data.Enum (enumFromTo)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Timer (clearInterval, setInterval, setTimeout, IntervalId)
import Prelude
import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make, send)
import React.Basic.DOM (table_, td_, text, tr_)
import React.Basic.DOM.Components.GlobalEvents (defaultOptions, windowEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, ctrlKey, shiftKey, code)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

import Level (CellType(..), Level, at, blockedBy, canGo)
import Sounds (moveSound, landSound)
import Types (Direction(..), GroundType(..), Point2D, Speed(..), moveTime, shiftBy)

component :: Component Props
component = createComponent "Game"

type Props = { audioContext :: AudioContext
             , buffers :: Map String AudioBuffer
             , level :: Level }
data Action = SetIntervalId IntervalId
            | KeyDown KeyboardEvent
            | Tick
            | Thaw
            | Restart

game :: Props -> JSX
game props = make component { initialState, update, render, didMount, willUnmount } props where
  initialState = { intervalId: Nothing
                 , position: (unwrap props.level).start
                 , canMove: true, step: 0, direction: Rightward }
  didMount self = send self <<< SetIntervalId =<<
    setInterval (unwrap self.props.level).updateInterval (send self Tick)
  update self = case _ of
    Tick -> NoUpdate
    KeyDown ke | self.state.canMove -> case code ke, shiftKey ke, ctrlKey ke of
      "ArrowLeft",  true,  false -> move self Leftward Fast
      "ArrowLeft",  false, true  -> jump self Leftward
      "ArrowLeft",  false, false -> move self Leftward Slow
      "ArrowRight", true,  false -> move self Rightward Fast
      "ArrowRight", false, true  -> jump self Rightward
      "ArrowRight", false, false -> move self Rightward Slow
      "ArrowUp",    true,  false -> move self Upward Fast
      "ArrowUp",    false, true  -> jump self Upward
      "ArrowUp",    false, false -> move self Upward Slow
      "ArrowDown",  true,  false -> move self Downward Fast
      "ArrowDown",  false, true  -> jump self Downward
      "ArrowDown",  false, false -> move self Downward Slow
      "Space",      false, false -> attack self
      "Enter",      false, false -> take self
      _, _, _ -> NoUpdate
    Thaw -> Update self.state { canMove = true }
    SetIntervalId id -> Update self.state { intervalId = Just id }
    Restart -> Update self.state { position = (unwrap self.props.level).start
                                 , canMove = true }
    _ -> NoUpdate
  render self
    = windowEvent { eventType: keydown
                  , handler: keydownHandler self
                  , options: defaultOptions } $
      plot self.props.level self.state.position { width: 15, height: 7 }
  willUnmount self = maybe (pure unit) clearInterval self.state.intervalId

  move self dir speed
    | dir /= self.state.direction = Update self.state { direction = dir }
    | canGo dir self.state.position self.props.level
    = case movingOn self dir of
      Just ground ->
        UpdateAndSideEffects self.state { step = (self.state.step + 1) `mod` 4
                                        , position = shiftBy dir 1 self.state.position
                                        , canMove = false } \_ -> do
          void $ setTimeout (moveTime ground speed) $ send self Thaw
          play self (moveSound ground speed $ self.state.step + 1) (pure unit)
      Nothing -> fall self dir
    | Just reason <- blockedBy dir self.state.position self.props.level
    = playSync self.state ["Angela_" <> reason]
    | otherwise = NoUpdate

  fall self dir =
    let newPos = shiftBy dir 1 self.state.position
        gravitate p n = let down = shiftBy Downward 1 p
                        in case at down self.props.level of
                          Floor _ -> n
                          _ -> gravitate down (n + 1)
        height = gravitate newPos 0
    in if height > 2
       then UpdateAndSideEffects self.state { canMove = false } \_ ->
         play self "Death_Fall" (send self Restart)
       else UpdateAndSideEffects
            self.state { canMove = false
                       , position = shiftBy Downward height newPos } \self' ->
            case standingOn self' of
              Just ground ->
                playAll self' ["Player_Jump", landSound ground] (send self' Thaw)  
              Nothing -> send self' Restart
  jump self dir = NoUpdate
  attack self = playSync self.state ["Player_Miss"]

  take self = NoUpdate

  keydownHandler self = maybe (pure unit) (send self <<< KeyDown) <<< fromEvent

standingOn :: forall state action
            . Self Props { position :: Point2D | state } action -> Maybe GroundType
standingOn self = case at self.state.position self.props.level of
  Floor Staircase -> Just Staircase
  _ -> case at (shiftBy Downward 1 self.state.position) self.props.level of
    Floor floor -> Just floor
    _ -> Nothing

movingOn :: forall state action
          . Self Props { position :: Point2D | state } action -> Direction
         -> Maybe GroundType
movingOn self dir = standingOn self { state { position = shiftBy dir 1 self.state.position } }

playAll :: forall state action
         . Self Props state action
        -> Array String
        -> Effect Unit
        -> Effect Unit
playAll self xs done = uncons xs # maybe done \{ head, tail } ->
  play self head (playAll self tail done)

play :: forall state action
      . Self Props state action
     -> String
     -> Effect Unit
     -> Effect Unit
play self sound done = case lookup sound self.props.buffers of
  Just buffer -> do
    source <- createBufferSource self.props.audioContext
    setBuffer buffer source
    connect source =<< destination self.props.audioContext
    onended source $ const done
    startBufferSource defaultStartOptions source
  Nothing -> done

playSync :: forall state
          . { canMove :: Boolean | state }
         -> Array String
         -> StateUpdate Props { canMove :: Boolean | state } Action
playSync state sounds = UpdateAndSideEffects
  state { canMove = false }
  \self -> playAll self sounds (send self Thaw)

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
