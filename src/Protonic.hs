module Protonic
  (
    Proto
  , ProtoT
  , runProtoT
  , withProtonic
  , runScene
  , Update, Render, Transit
  , Config (..), DebugJoystick (..)
  , defaultConfig
  , printsys
  , Scene (..)
  , SceneState (..)
  , Transition
  , continue, end, nextNew, next, pushNew, push
  --
  , screenSize
  -- Data
  , Font
  , Sprite (spsize)
  -- Render
  , setColor
  , clearBy
  , drawLine, drawRect, fillRect
  , renderS
  , printTest
  -- Sprite
  , newFont, freeFont
  , newSprite, freeSprite
  -- Metapad
  , Metapad
  , newPad
  , addAction
  , Input
  , hold, pressed, released
  , mousePosAct
  , Joystick
  , monitorJoystick
  , newJoystickAt, freeJoystick
  , joyHold, joyPressed, joyReleased
  , joyAxis, joyAxis2
  , joyAxisChanged, joyAxisChanged2
  , rumble
  -- * Sound
  , Sound
  , newSound
  , freeSound
  , play
  , playOn
  ) where

import           Protonic.Core    (Config (..), DebugJoystick (..), Proto, ProtoT,
                                   Render, Scene (..), SceneState (..), Transit,
                                   Transition, Update, continue, defaultConfig,
                                   end, next, nextNew, printsys, push, pushNew,
                                   runProtoT, runScene, withProtonic, screenSize)
import           Protonic.Data    (Font, Sprite (spsize))
import           Protonic.Metapad (Input, Joystick, Metapad, addAction,
                                   freeJoystick, hold, joyAxis, joyAxis2, joyAxisChanged, joyAxisChanged2,
                                   joyHold, joyPressed, joyReleased, monitorJoystick,
                                   mousePosAct, newJoystickAt, newPad, pressed,
                                   released, rumble)
import           Protonic.Render  (setColor, clearBy, drawLine, drawRect, fillRect, renderS, printTest)
import           Protonic.Sprite  (freeFont, freeSprite, newFont, newSprite)
import           Protonic.Sound   (Sound, newSound, freeSound, play, playOn)
