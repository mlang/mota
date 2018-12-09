module Web.Audio.Lens where

import Prelude ( class Monad
               , Unit
               , const, discard, flip
               , ($), (<<<), (<$>), (<*>), (>>=), (=<<))
import Data.Const (Const(..))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Audio.WebAudio.AudioParam (getValue, setValue)
import Audio.WebAudio.Types (PannerNode)
import Audio.WebAudio.PannerNode (orientationX, orientationY, orientationZ, positionX, positionY, positionZ, maxDistance, setMaxDistance) as Panner

type MutableLensM m s a b = forall f. Traversable f => (a -> f b) -> (s -> m (f Unit))
type MutableLensM' m s a = MutableLensM m s a a

mutableLensM :: forall m s a b
              . Monad m
             => (s -> m a) -> (s -> b -> m Unit)
             -> MutableLensM m s a b
mutableLensM g s f x = g x >>= traverse (s x) <<< f

mview :: forall m s a b. Monad m => MutableLensM m s a b -> s -> m a
mview l s = unwrap <$> l Const s

mset :: forall m s a b. Monad m => MutableLensM m s a b -> s -> b -> m Unit
mset l s v = unwrap <$> l (const $ Identity v) s

maxDistance :: MutableLensM' Effect PannerNode Number
maxDistance = mutableLensM Panner.maxDistance (flip Panner.setMaxDistance)

pannerOrientationX :: MutableLensM' Effect PannerNode Number
pannerOrientationX = mutableLensM (\s -> getValue =<< Panner.orientationX s)
                                  (\s x -> setValue x =<< Panner.orientationX s)
pannerOrientationY :: MutableLensM' Effect PannerNode Number
pannerOrientationY = mutableLensM (\s -> getValue =<< Panner.orientationY s)
                               (\s x -> setValue x =<< Panner.orientationY s)
pannerOrientationZ :: MutableLensM' Effect PannerNode Number
pannerOrientationZ = mutableLensM (\s -> getValue =<< Panner.orientationZ s)
                               (\s x -> setValue x =<< Panner.orientationZ s)
pannerPositionX :: MutableLensM' Effect PannerNode Number
pannerPositionX = mutableLensM (\s -> getValue =<< Panner.positionX s)
                               (\s x -> setValue x =<< Panner.positionX s)
pannerPositionY :: MutableLensM' Effect PannerNode Number
pannerPositionY = mutableLensM (\s -> getValue =<< Panner.positionY s)
                               (\s x -> setValue x =<< Panner.positionY s)
pannerPositionZ :: MutableLensM' Effect PannerNode Number
pannerPositionZ = mutableLensM (\s -> getValue =<< Panner.positionZ s)
                               (\s x -> setValue x =<< Panner.positionZ s)

pannerOrientation :: MutableLensM' Effect PannerNode { x::Number, y::Number, z::Number }
pannerOrientation = mutableLensM get set where
  get s = { x:_, y:_, z:_ } <$> mview pannerOrientationX s
                            <*> mview pannerOrientationY s
                            <*> mview pannerOrientationZ s
  set s p = do
    mset pannerOrientationX s p.x
    mset pannerOrientationY s p.y
    mset pannerOrientationZ s p.z

pannerPosition :: MutableLensM' Effect PannerNode { x::Number, y::Number, z::Number }
pannerPosition = mutableLensM get set where
  get s = { x:_, y:_, z:_ } <$> mview pannerPositionX s
                            <*> mview pannerPositionY s
                            <*> mview pannerPositionZ s
  set s p = do
    mset pannerPositionX s p.x
    mset pannerPositionY s p.y
    mset pannerPositionZ s p.z
