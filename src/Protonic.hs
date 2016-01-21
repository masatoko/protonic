module Protonic
  (
    ProtoT
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
  , clearBy
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
  , joyHold, joyPressed, joyAxisChanged, joyAxisChanged2
  ) where

import           Protonic.Core    (Config (..), DebugJoystick (..), ProtoT,
                                   Render, Scene (..), SceneState (..), Transit,
                                   Transition, Update, continue, defaultConfig,
                                   end, next, nextNew, printsys, push, pushNew,
                                   runProtoT, runScene, withProtonic, screenSize)
import           Protonic.Data    (Font, Sprite (spsize))
import           Protonic.Metapad (Input, Joystick, Metapad, addAction,
                                   freeJoystick, hold, joyAxisChanged, joyAxisChanged2,
                                   joyHold, joyPressed, monitorJoystick,
                                   mousePosAct, newJoystickAt, newPad, pressed,
                                   released)
import           Protonic.Render  (clearBy, printTest, renderS)
import           Protonic.Sprite  (freeFont, freeSprite, newFont, newSprite)
