module Protonic
  (
    ProtoT
  , runProtoT
  , withProtonic
  , runGame
  --
  , frame
  -- Render
  , clearBy
  , testText
  -- Sprite
  , newFont, freeFont
  , newSprite, freeSprite
  ) where

import Protonic.Core (ProtoT, runProtoT, withProtonic, runGame, frame)
import Protonic.Render (clearBy, testText)
import Protonic.Sprite (newFont, freeFont, newSprite, freeSprite)
