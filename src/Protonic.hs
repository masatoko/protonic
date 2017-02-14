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
  , continue, end, next, push
  --
  , screenSize
  , getWindow
  , averageTime
  , setRendererDrawBlendMode
  -- Data
  , Font
  , Sprite (spsize)
  -- Render
  , setColor
  , clearBy
  , drawLine, drawRect, fillRect
  , renderS, renderS'
  , printTest
  -- Sprite
  , newFont, freeFont
  , ascent, descent
  , newSprite, loadSprite, decodeSprite, freeSprite
  , setBlendMode, setAlphaMod, setColorMod
  , GlyphMetrics (..), glyphMetrics
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
  , mouseWheelAct
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
  , joyAllHat
  , rumble
  ) where

import           Protonic.Core
import           Protonic.Data
import           Protonic.Metapad
import           Protonic.Render
import           Protonic.Sprite
