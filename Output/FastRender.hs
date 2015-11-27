module Output.FastRender where

import Output.Render
import qualified Instruments

{-
This module is separate from Output.Render
in order to avoid a loop of imports
-}

writeScore = putStrLn . allRendering
quickScore m = writeScore $ m & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.melody
    )