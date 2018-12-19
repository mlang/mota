module Components.Motar where

import Affjax.ResponseFormat (printResponseFormatError)
import Audio.WebAudio.Types (AudioBuffer, AudioContext)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (either)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Prelude (discard, otherwise, pure, ($), (<<<), (>>=))
import React.Basic
import React.Basic.DOM (h1_, text)

import Components.Game (game)
import Level (Level, grab)
import Sounds (loadSounds, sounds)

component :: Component Props
component = createComponent "Motar"

type Props = { audioContext :: AudioContext, updateInterval :: Int }
data Action = SetSounds (Map String AudioBuffer)
            | SetLevel Level

motar :: Props -> JSX
motar = make component { initialState, update, render, didMount } where
  initialState = { buffers: Nothing, level: Nothing }
  didMount self = do
    sendAsync self $
      loadSounds self.props.audioContext sounds >>=
      either (throwError <<< error <<< printResponseFormatError)
             (pure <<< SetSounds)
    sendAsync self $
      runExceptT (grab "level0.json") >>=
        either (throwError <<< error) (pure <<< SetLevel)
  update self = case _ of
    SetSounds buffers -> Update self.state { buffers = Just buffers }
    SetLevel level -> Update self.state { level = Just level }
  render self
    | Just buffers <- self.state.buffers
    , Just level <- self.state.level
    = game { audioContext: self.props.audioContext, buffers, level }
    | otherwise
    = h1_ [text "Loading, please wait..."]
