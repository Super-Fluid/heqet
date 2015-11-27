module LyInstances where

-- for the Ly instances:
import Types
import qualified Tables
import Output.Templates

import Data.Tuple
import Control.Lens
import Safe

instance Playable LyPitch where
    info (LyPitch p) = Just $ PlayInfo { 
    _slurrable = True
  , _chordable = True
  , _pitchHeight = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
    }

instance Playable LyRest where
    info _ = Just $ PlayInfo False False (Just 0)

instance Playable LyPerc where
    info _ = Just $ PlayInfo True True (Just 0)  -- something better here...

instance Playable LyEffect where
    info _ = Just $ PlayInfo True False (Just 0)

instance Playable LyLyric where
    info _ = Just $ PlayInfo True False Nothing

instance Playable LyGrace where
    info p = Just $ PlayInfo {
      _slurrable = False -- for now
    , _chordable = False
    , _pitchHeight = Just 0 -- IMPROVE!
    }

instance Playable LyMeasureEvent where
    info _ = Nothing

instance Playable LyBeatEvent where
    info _ = Nothing

instance Playable LyKeyEvent where
    info _ = Nothing

instance Playable LyClefEvent where
    info _ = Nothing

instance Playable LyMeterEvent where
    info _ = Nothing
