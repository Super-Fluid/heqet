module LyInstances where

-- for the Ly instances:
import Types
import qualified Tables
import Data.Tuple

import Control.Lens
import Safe

instance Ly'' LyPitch where
    renderInStaff n (LyPitch p) = renderPitchAcc (p^.pc) (n^.getnote.acc) ++ renderOct (p^.oct)
    getMarkup _ = []
    slurrable _ = True
    chordable _ = True
    pitchHeight (LyPitch p) = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
    comparable _ = True
    takesUpTime _ = True

renderPitchAcc :: PitchClass -> (Maybe Accidental) -> String
renderPitchAcc pc (Just acc) = fromJustNote "renderPitchAcc (with acc)" $ lookup (pc,acc) (map swap Tables.en)
renderPitchAcc pc Nothing = fromJustNote "renderPitchAcc (no acc)" $ lookup pc (map (\((p,a),s) -> (p,s)) (map swap Tables.en))

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''


{- 
instance Ly'' LyRest where
    renderInStaff n ly =
    getMarkup _ = []
    slurrable _ = False
    chordable _ = False
    pitchHeight ly = Just 0
    comparable ly = 
    takesUpTime _ = True

instance Ly'' LyPercwhere
    renderInStaff n ly =
    getMarkup (LyPerc s) = [markupText s]
    slurrable _ = True
    chordable _ = True
    pitchHeight ly = Just 0 -- something better here...
    comparable ly = 
    takesUpTime _ = True

instance Ly'' LyEffect where
    renderInStaff n ly =
    getMarkup _ = []
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Just 0
    comparable ly = 
    takesUpTime _ = True

instance Ly'' LyLyric where
    renderInStaff n ly =
    getMarkup (LyLyric s) = [markupText s]
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Nothing
    comparable ly = 
    takesUpTime _ = True

instance Ly'' LyGrace where
    renderInStaff n ly =
    getMarkup _ = []
    slurrable _ = False -- for now
    chordable _ = False
    pitchHeight ly = Just 0 -- IMPROVE!
    comparable ly = 
    takesUpTime _ = True

instance Ly'' LyMeasureEvent where
    renderInStaff n ly =
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    comparable ly = 
    takesUpTime _ = False

instance Ly'' LyBeatEvent where
    renderInStaff n ly =
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    comparable ly = 
    takesUpTime _ = False

instance Ly'' LyBeatEvent where
    renderInStaff n ly =
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    comparable ly = 
    takesUpTime _ = False

instance Ly'' LyClefEvent where
    renderInStaff n ly =
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    comparable ly = 
    takesUpTime _ = False

instance Ly'' LyMeterEvent where
    renderInStaff n ly =
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    comparable ly = 
    takesUpTime _ = False
-}