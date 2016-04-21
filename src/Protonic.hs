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
  , averageTime
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
  , MouseButton (..)
  , InputMotion (..)
  , HatDir (..)
  , hold, pressed, released
  , mousePosAct
  , mouseMotionAct
  , mouseButtonAct
  , touchMotionAct
  , Joystick
  , monitorJoystick
  , newJoystickAt, freeJoystick
  , joyHold, joyPressed, joyReleased
  , joyAxis, joyAxis2
  , joyAxisChanged, joyAxisChanged2
  , joyAllButtons
  , joyAllAxes
  , joyHat
  , rumble
  ) where

import           Protonic.Core    (Config (..), DebugJoystick (..), Proto, ProtoT,
                                   Render, Scene (..), SceneState (..), Transit,
                                   Transition, Update, continue, defaultConfig,
                                   end, next, nextNew, printsys, push, pushNew,
                                   runProtoT, runScene, withProtonic, screenSize,
                                   averageTime)
import           Protonic.Data    (Font, Sprite (spsize))
import           Protonic.Metapad (Input, Joystick, Metapad, MouseButton (..),
                                   InputMotion (..), HatDir (..), addAction,
                                   freeJoystick, hold, joyAxis, joyAxis2, joyAxisChanged, joyAxisChanged2,
                                   joyHold, joyPressed, joyReleased, joyAllButtons, joyAllAxes, joyHat, monitorJoystick,
                                   mousePosAct, mouseMotionAct, mouseButtonAct, touchMotionAct, newJoystickAt, newPad, pressed,
                                   released, rumble)
import           Protonic.Render  (setColor, clearBy, drawLine, drawRect, fillRect, renderS, printTest)
import           Protonic.Sprite  (freeFont, freeSprite, newFont, newSprite)
