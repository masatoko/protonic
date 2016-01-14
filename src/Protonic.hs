module Protonic
  (
    ProtoT
  , runProtonic
  --
  , frame
  -- Render
  , clearBy
  , testText
  ) where

import Protonic.Core (ProtoT, runProtonic, frame)
import Protonic.Render (clearBy, testText)
import Protonic.Sprite (newFont, freeFont, newSprite, freeSprite)
