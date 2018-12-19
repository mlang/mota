module Main where

import Prelude

import Components.Motar (motar)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  root <- getElementById "root" =<<
          map toNonElementParentNode (document =<< window)
  case root of
    Nothing -> throw "Root element not found."
    Just container -> do
      audioContext <- newAudioContext
      render (motar { audioContext, updateInterval: 100 }) container
