module Protonic.Scene where

import Protonic.Metapad
import Protonic.Core

data Scene app a = Scene
  { scenePad :: Metapad a
  , sceneUpdate :: [a] -> app -> ProtoT app
  , sceneRender :: app -> ProtoT ()
  }
