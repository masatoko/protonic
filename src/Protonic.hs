module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runGame
  , Config (..)
  , defaultConfig
  , printsys
  --
  , frame
  -- Data
  , Font
  , Sprite
  -- Render
  , clearBy
  , renderS
  , testText
  -- Sprite
  , newFont, freeFont
  , newSprite, freeSprite
  -- Metapad
  , Metapad
  , newPad
  , addAction
  , Input
  , keyAct
  , mousePosAct
  ) where

import           Protonic.Core   (Config (..), ProtoT, defaultConfig, frame,
                                  runGame, runProtoT, withProtonic, printsys)
import           Protonic.Data   (Font, Sprite)
import           Protonic.Render (clearBy, renderS, testText)
import           Protonic.Sprite (freeFont, freeSprite, newFont, newSprite)
import           Protonic.Metapad (Metapad, newPad, addAction, Input , keyAct, mousePosAct)
