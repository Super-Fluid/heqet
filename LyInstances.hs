module LyInstances where

-- for the Ly instances:
import Types
import qualified Tables
import Output.Templates
import Output.RenderTypes

import Data.Tuple
import Control.Lens
import Safe


instance Playable LyPitch where
    slurrable _ = True
    chordable _ = True
    pitchHeight (LyPitch p) = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
    takesUpTime _ = True

instance Playable LyRest where
    slurrable _ = False
    chordable _ = False
    pitchHeight ly = Just 0
    takesUpTime _ = True

instance Playable LyPerc where
    slurrable _ = True
    chordable _ = True
    pitchHeight ly = Just 0 -- something better here...
    takesUpTime _ = True

instance Playable LyEffect where
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Just 0
    takesUpTime _ = True

instance Playable LyLyric where
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Nothing
    takesUpTime _ = True

instance Playable LyGrace where
    slurrable _ = False -- for now
    chordable _ = False
    pitchHeight ly = Just 0 -- IMPROVE!
    takesUpTime _ = True

-- DELETE vvvv
instance Playable LyMeasureEvent where
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Playable LyBeatEvent where
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Playable LyKeyEvent where
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Playable LyClefEvent where
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Playable LyMeterEvent where
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False
