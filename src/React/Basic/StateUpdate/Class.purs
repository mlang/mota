module React.Basic.StateUpdate.Class (
  class MonadStateUpdate, sideEffects, action, delayedAction
) where

import Prelude
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State.Class (class MonadState)
import Effect (Effect)
import Effect.Timer (setTimeout)
import React.Basic (Self, send)

class ( MonadAsk props monad
      , MonadState state monad
      ) <= MonadStateUpdate props state action monad
         | monad -> props state action where
  sideEffects :: (Self props state action -> Effect Unit) -> monad Unit

action
  :: forall props state action monad. MonadStateUpdate props state action monad
  => action -> monad Unit
action = sideEffects <<< flip send

delayedAction
  :: forall props state action monad. MonadStateUpdate props state action monad
  => Int -> action -> monad Unit
delayedAction ms a = sideEffects \self -> void $ setTimeout ms $ send self a
