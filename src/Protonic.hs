module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runGame
  , Config (..)
  , defaultConfig
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
  ) where

import           Protonic.Core   (Config (..), ProtoT, defaultConfig, frame,
                                  runGame, runProtoT, withProtonic)
import           Protonic.Data   (Font, Sprite)
import           Protonic.Render (clearBy, renderS, testText)
import           Protonic.Sprite (freeFont, freeSprite, newFont, newSprite)
