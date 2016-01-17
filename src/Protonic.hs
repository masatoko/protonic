module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runScene
  , Update, Render
  , Config (..)
  , defaultConfig
  , printsys, printsys'
  , Scene (..)
  , Transition (..)
  --
  , frame
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
  ) where

import           Protonic.Core    (Config (..), ProtoT, defaultConfig, frame,
                                   printsys, printsys', runScene, runProtoT,
                                   withProtonic, Update, Render, Scene (..), Transition (..))
import           Protonic.Data    (Font, Sprite)
import           Protonic.Metapad (Input, Metapad, addAction, hold, pressed, released,
                                   mousePosAct, newPad)
import           Protonic.Render  (clearBy, renderS, printTest)
import           Protonic.Sprite  (freeFont, freeSprite, newFont, newSprite)
