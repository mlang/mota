module React.Basic.StateUpdate.Monad (StateUpdateM, runStateUpdateM) where

import Prelude
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.RWS (RWS, execRWS)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Writer.Class (tell)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic (Self, StateUpdate(..))
import React.Basic.StateUpdate.Class

newtype StateUpdateM props state action a = StateUpdateM (RWS
  props (Array (Self props state action -> Effect Unit)) (Either state state) a
)

runStateUpdateM
  :: forall p s a. Self p s a -> StateUpdateM p s a Unit -> StateUpdate p s a
runStateUpdateM self (StateUpdateM m) =
  case execRWS m self.props (Left self.state) of
    Tuple (Left _) [] -> NoUpdate
    Tuple (Left _) effs -> SideEffects $ fold effs
    Tuple (Right state) [] -> Update state
    Tuple (Right state) effs -> UpdateAndSideEffects state $ fold effs

instance functorStateUpdateM :: Functor (StateUpdateM p s a) where
  map f (StateUpdateM m) = StateUpdateM $ map f m

instance applyStateUpdateM :: Apply (StateUpdateM p s a) where
  apply (StateUpdateM f) (StateUpdateM m) = StateUpdateM $ apply f m

instance applicativeStateUpdateM :: Applicative (StateUpdateM p s a) where
  pure = StateUpdateM <<< pure

instance bindStateUpdateM :: Bind (StateUpdateM p s a) where
  bind (StateUpdateM m) f = StateUpdateM $ m >>= \a ->
    case f a of StateUpdateM m' -> m'

instance monadStateUpdateM :: Monad (StateUpdateM p s a)

instance monadAskStateUpdateM :: MonadAsk p (StateUpdateM p s a) where
  ask = StateUpdateM ask

instance monadStateStateUpdateM :: MonadState s (StateUpdateM p s a) where
  state f = StateUpdateM <<< state $ map Right <<< either f f

instance monadStateUpdateStateUpdateM :: MonadStateUpdate p s a (StateUpdateM p s a) where
  sideEffects eff = StateUpdateM $ tell [eff]
