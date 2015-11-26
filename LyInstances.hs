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
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyPercwhere
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyEffect where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyLyric where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyGrace where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyMeasureEvent where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyBeatEvent where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyBeatEvent where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyClefEvent where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 

instance Ly'' LyMeterEvent where
    renderInStaff n ly =
    getMarkup ly = 
    slurrable _ = 
    chordable _ = 
    pitchHeight ly = 
    comparable ly = 
-}