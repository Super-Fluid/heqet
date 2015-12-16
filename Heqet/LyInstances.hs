module Heqet.LyInstances where

-- for the Ly instances:
import Heqet.Types
import qualified Heqet.Tables as Tables
import Heqet.Output.Templates

import Data.Tuple
import Control.Lens
import Safe

pitch2double :: Pitch -> Double
pitch2double p = (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)

instance Playable LyPitch where
    info (LyPitch p) = Just $ PlayInfo { 
    _slurrable = True
  , _chordable = True
  , _pitchHeight = Just $ pitch2double p
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
