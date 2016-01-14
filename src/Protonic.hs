module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runGame
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

import Protonic.Core (ProtoT, runProtoT, withProtonic, runGame, frame)
import Protonic.Data (Font, Sprite)
import Protonic.Render (clearBy, renderS, testText)
import Protonic.Sprite (newFont, freeFont, newSprite, freeSprite)
