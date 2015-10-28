{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Render where

import Types
import qualified Tables
import Templates

import Control.Lens
import Data.Maybe (fromJust,isJust)
import Data.Tuple (swap)
import Data.List (concat, intersperse)

testRender :: Music -> String
testRender mu = "\\version \"2.16.2\"\n\\language \"english\"\n\\score {\n\\new Staff \\with {\nmidiInstrument = \"bassoon\"\n} { \n\\once \\override Staff.TimeSignature #'stencil = ##f \n\\clef bass\n\\cadenzaOn " ++ (listRender mu) ++ "\n\\cadenzaOff\n \\bar \"|\"\n}\\layout { }\n\\midi { }\n}"

listRender :: Music -> String
listRender intimes = concat $ intersperse " " $ map toLy $ map (\i -> (i^.val,i^.dur)) intimes

toLy (note,dur) = renderPitch (note^.pitch) (note^.acc) ++ renderDuration (dur)

renderPitch :: Ly -> (Maybe Accidental) -> String
renderPitch (Pitch p) acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)
renderPitch Rest _ = "r"
renderPitch (Perc p) _ = "c"

renderPitchAcc :: PitchClass -> (Maybe Accidental) -> String
renderPitchAcc pc (Just acc) = fromJust $ lookup (pc,acc) (map swap Tables.en)
renderPitchAcc pc Nothing = fromJust $ lookup pc (map (\((p,a),s) -> (p,s)) (map swap Tables.en))

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

data WrittenNote = WrittenNote { 
      _preceeding :: [String]
    , _body :: String
    , _duration :: Duration
    , _noteItems :: [String]
    , _following :: [String]
    }
    deriving (Show)
makeLenses ''WrittenNote

type NoteInProgress = (Note Ly, WrittenNote)

startRenderingNote :: InTime (Note Ly) -> NoteInProgress
startRenderingNote it = (it^.val, WrittenNote [] "" (it^.dur) [] [])

renderNoteBodyInStaff :: NoteInProgress -> NoteInProgress
renderNoteBodyInStaff (n, w) = let
    body' = case n^.pitch of 
        Pitch p -> renderPitch' p (n^.acc)
        Perc s -> xNote n
        Rest -> "r"
        Effect -> xNote n
        Lyric s -> xNote n
        Grace mus -> "\\grace {" ++ renderInStaff mus ++ "}"
    nis' = case n^.pitch of
        Pitch _ -> w^.noteItems 
        Perc s -> (markupText s):(w^.noteItems)
        Rest -> w^.noteItems
        Effect -> w^.noteItems
        Lyric s -> (markupText s):(w^.noteItems)
        Grace mus -> w^.noteItems
    in (n, w & noteItems .~ nis' & body .~ body')

renderInStaff :: Music -> String
renderInStaff mus = concat $ intersperse " " $ map f mus where
    f note = note
        & startRenderingNote
        & renderNoteBodyInStaff
        & extractRenderedNote

extractRenderedNote :: NoteInProgress -> String
extractRenderedNote (n,w) = 
    (concat $ w^.preceeding)
    ++ (w^.body)
    ++ (renderDuration $ w^.duration)
    ++ (concat $ w^.noteItems)
    ++ (concat $ reverse $ w^.following)

xNote :: Note Ly -> String
xNote n = let 
    fakePitch = case n^.clef of
        Just Treble  -> "b'"
        Just Alto    -> "c'"
        Just Treble8 -> "b"
        Just Tenor   -> "a"
        Just Bass    -> "d"
        _            -> "c'"
    in "\\xNote "++fakePitch

renderPitch' :: Pitch -> (Maybe Accidental) -> String
renderPitch' p acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)