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
    takesUpTime _ = True

renderPitchAcc :: PitchClass -> (Maybe Accidental) -> String
renderPitchAcc pc (Just acc) = fromJustNote "renderPitchAcc (with acc)" $ lookup (pc,acc) (map swap Tables.en)
renderPitchAcc pc Nothing = fromJustNote "renderPitchAcc (no acc)" $ lookup pc (map (\((p,a),s) -> (p,s)) (map swap Tables.en))

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''

instance Ly'' LyRest where
    renderInStaff _ _ = "r"
    getMarkup _ = []
    slurrable _ = False
    chordable _ = False
    pitchHeight ly = Just 0
    takesUpTime _ = True

instance Ly'' LyPerc where
    renderInStaff n _ = xNote n
    getMarkup (LyPerc s) = [markupText s]
    slurrable _ = True
    chordable _ = True
    pitchHeight ly = Just 0 -- something better here...
    takesUpTime _ = True

xNote :: Note MultiPitchLy -> String
xNote n = let 
    fakePitch = case n^.clef of
        Just Treble  -> "b'"
        Just Alto    -> "c'"
        Just Treble8 -> "b"
        Just Tenor   -> "a"
        Just Bass    -> "d"
        _            -> "c'"
    in "\\xNote "++fakePitch

instance Ly'' LyEffect where
    renderInStaff n _ = xNote n
    getMarkup _ = []
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Just 0
    takesUpTime _ = True

instance Ly'' LyLyric where
    renderInStaff n _ = xNote n
    getMarkup (LyLyric s) = [markupText s]
    slurrable _ = True
    chordable _ = False
    pitchHeight ly = Nothing
    takesUpTime _ = True

instance Ly'' LyGrace where
    renderInStaff n (LyGrace mus) = "\\grace {" ++ allRenderingForGrace n mus ++ "}"
    getMarkup _ = []
    slurrable _ = False -- for now
    chordable _ = False
    pitchHeight ly = Just 0 -- IMPROVE!
    takesUpTime _ = True

instance Ly'' LyMeasureEvent where
    renderInStaff _ _ = " | "
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Ly'' LyBeatEvent where
    renderInStaff _ _ = ""
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Ly'' LyBeatEvent where
    renderInStaff _ _ = ""
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Ly'' LyClefEvent where
    renderInStaff _ (LyClefEvent c) = "\\clef " ++ clef ++ " " where
        clef = case c of
            Treble -> "treble"
            Alto -> "alto"
            Tenor -> "tenor"
            Bass -> "bass"
            Treble8 -> "\"treble_8\""
            CustomClef s -> s -- let's hope the user knows what they're doing
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False

instance Ly'' LyMeterEvent where
    renderInStaff _ (LyMeterEvent (Meter num denom)) = "\\time " ++ show num ++ "/" ++ show denom ++ " "
    getMarkup _ = undefined
    slurrable _ = undefined
    chordable _ = undefined
    pitchHeight _ = undefined
    takesUpTime _ = False
