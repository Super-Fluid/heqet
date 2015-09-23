module Render where

import Types
import qualified Tables

import Control.Lens
import Data.Maybe (fromJust,isJust)
import Data.Tuple (swap)
import Data.List (concat, intersperse)

testRender :: Music' Note -> String
testRender mu = "\\version \"2.16.2\"\n\\language \"english\"\n\\score {\n\\new Staff \\with {\nmidiInstrument = \"bassoon\"\n} { \n\\once \\override Staff.TimeSignature #'stencil = ##f \n\\clef bass\n\\cadenzaOn " ++ (listRender mu) ++ "\n\\cadenzaOff\n \bar \"|\"\n}\\layout { }\n\\midi { }\n}"

listRender :: Music' Note -> String
listRender intimes = concat $ intersperse " " $ map toLy $ map (\i -> (i^.val,i^.dur)) intimes

toLy (note,dur) = renderPitch (note^.pitch) (note^.acc) ++ renderDuration (dur)

renderPitch :: Pitch' -> Accidental -> String
renderPitch (RegPitch p) acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)
renderPitch Rest _ = "r"
renderPitch (Perc p) _ = "c"

renderPitchAcc :: PitchClass -> Accidental -> String
renderPitchAcc pc acc = fromJust $ lookup (pc,acc) (map swap Tables.en)

commonDurations :: [(Duration,String)]
commonDurations = [
     (1,"4")
    ,(2,"2")
    ,(4,"1")
    ,(8,"\\breve")
    ,(1/2,"8")
    ,(1/4,"16")
    ,(1/8,"32")
    ,(1/16,"64")
    ,(1/32,"128")
    ,(1/64,"256")
    ,(3/2,"4.")
    ,(3,"2.")
    ,(6,"1.")
    ,(12,"\\breve.")
    ,(3/4,"8.")
    ,(3/8,"16.")
    ,(3/16,"32.")
    ,(3/32,"64.")
    ,(3/64,"128.")
    ,(3/128,"256.")
     ]

renderDuration :: Duration -> String
renderDuration dur
    | isJust (lookup dur commonDurations) = fromJust (lookup dur commonDurations)
    | otherwise = "1"

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''