module Heqet.Output.FastRender where

import Heqet.Output.Render
import qualified Heqet.Instruments as Instruments
import Heqet.Types
import Heqet.Tools
import Heqet.Split

import Control.Lens

{-
This module is separate from Output.Render
in order to avoid a loop of imports
-}

writeScore = putStrLn . allRendering
quickScore m = writeScore $ superBasicSplit $ m & traverse.val.inst .~ Just Instruments.piano
quickLine m = writeScore $ m & traverse.val.inst .~ Just Instruments.melody