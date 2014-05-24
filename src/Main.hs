{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Main module, entry point
module Main (main) where

import Prelude ()
import BasicPrelude as Base

import Control.Concurrent (threadDelay)
import Control.Eff
import Control.Eff.Lift as Eff
import Control.Eff.State.Strict
import Data.Conduit
import Data.Conduit.List as Conduit
import Data.HashMap.Strict (fromList)
import Sound.MIDI
import Wrappers.GLFW
import Wrappers.Events

import Main.Graphics
import Main.Input

import Input
import Logic

drumc :: Channel
drumc = 9

windowSize :: Num a => (a, a)
windowSize = (64, 64)

-- | Title of the game window
title :: String
title = "Soundflow"

-- | Output MIDI destinations
outMIDI :: [String]
outMIDI = ["128:0"]

-- | Map MIDI sources to instruments
inMIDI :: HashMap String Instrument
inMIDI = fromList []

-- | Entry point
main :: IO ()
main = do
      putStrLn "initializing GLFW"
      result <- runGLFW title Nothing (0, 0 :: Integer) windowSize
        $ \wnd -> do
          putStrLn "GLFW initialized"
          putStrLn "initializing events"
          initEvents wnd
          putStrLn "events initialized"
          putStrLn "initializing MIDI"
          void
            $ runMIDI title outMIDI inMIDI drumc
            $ \midiState -> void
                          $ runLift
                          $ runState midiState
                          $ do
                              putStrLn "MIDI initialized"
                              tempo (div 60000000 bpm)
                              mainLoop wnd inputs
                              putStrLn "done MIDI"
      maybe (putStrLn "error initializing GLFW") return result
      putStrLn "closing"
    where
        mainLoop ::
            (Functor (Eff env), Monad (Eff env), MIDI env) =>
            Window ->
            Source (Eff env) Input ->
            Eff env ()
        mainLoop wnd inputSource
            =  inputSource
            $= Conduit.mapM (\x -> x <$ Eff.lift (putStrLn $ "input to logic " <> show x))
            $= logic
            $= Conduit.mapM (\x -> x <$ Eff.lift (putStrLn $ "output from logic " <> show x))
            $$ awaitForever (ioUpdates wnd)

        ioUpdates wnd note = Base.lift $ do
            sendMIDI note
            liftIO $ updateGraphics wnd
            liftIO $ threadDelay 10000
            flush

        sendMIDI :: MIDI env => (Note, Maybe Velocity) -> Eff env ()
        sendMIDI (note, Just v) = startNote v note
        sendMIDI (note, Nothing) = stopNote   note
