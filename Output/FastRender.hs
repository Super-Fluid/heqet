module Output.FastRender where

import Output.Render
import qualified Instruments
import Types
import Tools
import Split

import Control.Lens

{-
This module is separate from Output.Render
in order to avoid a loop of imports
-}

writeScore = putStrLn . allRendering
quickScore m = writeScore $ superBasicSplit $ m & traverse.val.inst .~ Just Instruments.piano
quickLine m = writeScore $ m & traverse.val.inst .~ Just Instruments.melody