module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runScene
  , Update, Render
  , Config (..)
  , defaultConfig
  , printsys
  , Scene (..)
  , SceneState (..)
  , Transition
  , continue, end, nextNew, next, pushNew, push
  -- Data
  , Font
  , Sprite
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
  , withJoystickAt
  , joyPressed
  ) where

import           Protonic.Core    (Config (..), ProtoT, Render, Scene (..), SceneState (..), Transition,
                                   continue, end, nextNew, next, pushNew, push,
                                   Update, defaultConfig,
                                   printsys, runProtoT,
                                   runScene, withProtonic)
import           Protonic.Data    (Font, Sprite)
import           Protonic.Metapad (Input, Metapad, addAction, hold, mousePosAct,
                                   newPad, pressed, released, Joystick, withJoystickAt, joyPressed)
import           Protonic.Render  (clearBy, printTest, renderS)
import           Protonic.Sprite  (freeFont, freeSprite, newFont, newSprite)
