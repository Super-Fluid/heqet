{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.Render where

import Types
import qualified Tables
import Output.Templates
import Tools
import Output.RenderTypes
import List
import qualified Output.LilypondSettings
import qualified Instruments

import Control.Lens
import Data.Maybe
import Data.Tuple
import Data.List
import Control.Applicative
import Data.Monoid
import Safe

toLy (note,dur) = renderPitch (note^.pitch) (note^.acc) ++ renderDuration (dur)

renderPitch :: Ly -> (Maybe Accidental) -> String
renderPitch (Pitch p) acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)
renderPitch Rest _ = "r"
renderPitch (Perc p) _ = "c"

renderPitchAcc :: PitchClass -> (Maybe Accidental) -> String
renderPitchAcc pc (Just acc) = fromJustNote "renderPitchAcc (with acc)" $ lookup (pc,acc) (map swap Tables.en)
renderPitchAcc pc Nothing = fromJustNote "renderPitchAcc (no acc)" $ lookup pc (map (\((p,a),s) -> (p,s)) (map swap Tables.en))

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
    | isJust (lookup dur commonDurations) = fromJustNote "renderDuration" (lookup dur commonDurations)
    | otherwise = "1"

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''

startRenderingNote :: InTime (Note MultiPitchLy) -> NoteInProgress
startRenderingNote it = (it^.val, WrittenNote [] "" (it^.dur) [] []) -- TODO: INSTR{UMENTS

renderNoteBodyInStaff :: NoteInProgress -> NoteInProgress
renderNoteBodyInStaff (n, w) = let
    body' = case n^.pitch of 
        OneLy ly -> renderOneNoteBodyInStaff n ly
        ManyLy lys -> renderManyNoteBodiesInStaff n lys
    nis' = case n^.pitch of
        OneLy ly -> (getMarkupFromOneLy ly)++(w^.noteItems)
        ManyLy lys -> (getMarkupFromManyLys lys)++(w^.noteItems)
    in (n, w & noteItems .~ nis' & body .~ body')

renderOneNoteBodyInStaff :: (Note MultiPitchLy) -> (Ly,Maybe Accidental,Maybe Instrument) -> String
renderOneNoteBodyInStaff n (ly,a,_) = case ly of
    Pitch p -> renderPitch' p a
    Perc s -> xNote n
    Rest -> "r"
    Effect -> xNote n
    Lyric s -> xNote n
    Grace mus -> "\\grace {" ++ allRendering mus ++ "}"

renderManyNoteBodiesInStaff :: (Note MultiPitchLy) -> [(Ly,Maybe Accidental,Maybe Instrument)] -> String
renderManyNoteBodiesInStaff n lys = "< " ++ (concat $ intersperse " " $ map (renderOneNoteBodyInStaff n) lys) ++ " >"

{-
Get markup to render Ly types that we don't have a better way to render.
-}
getMarkupFromOneLy :: (Ly,Maybe Accidental,Maybe Instrument) -> [String]
getMarkupFromOneLy (ly,_,_) = case ly of
    Pitch _ -> []
    Perc s -> [markupText s]
    Rest -> []
    Effect -> []
    Lyric s -> [markupText s]
    Grace mus -> []

getMarkupFromManyLys :: [(Ly,Maybe Accidental,Maybe Instrument)] -> [String]
getMarkupFromManyLys = concat . map getMarkupFromOneLy

renderNoteItems :: NoteInProgress -> NoteInProgress
renderNoteItems (n,w) = let
    beginExpr = map (^.begin) (n^.exprCommands)
    endExpr = map (^.end) (n^.exprCommands)
    beginNDExpr = map (^.begin) (n^.nonDistCommands)
    endNDExpr = map (^.end) (n^.nonDistCommands)
    markedErrors = map markupText $ n^.errors
    articulations = map renderArt $ n^.artics
    w' = w
        & preceeding .~ beginExpr ++ beginNDExpr
        & following .~ endExpr ++ endNDExpr
        & noteItems .~ (n^.noteCommands) ++ articulations ++ markedErrors
    in (n,w')

applySlurs :: [NoteInProgress] -> [NoteInProgress]
applySlurs ns = reverse $ snd $ foldl applySlurs'h (False,[]) ns where
    applySlurs'h :: (Bool,[NoteInProgress]) -> NoteInProgress -> (Bool,[NoteInProgress])
    applySlurs'h (isInSlur,acc) note
        | isInSlur && note^._1.isSlurred = (True,(note:acc))
        | isInSlur && otherwise = (False,(note & _2.noteItems %~ (")":) ):acc)
        | otherwise && note^._1.isSlurred = (True,(note & _2.noteItems %~ ("(":) ):acc)
        | otherwise && otherwise = (False,(note:acc))

renderArt :: SimpleArticulation -> String
renderArt Staccato = "-."
renderArt Marcato = "-^"
renderArt Tenuto = "--"
renderArt Portato = "-_"
renderArt Staccatissimo = "-|"
renderArt Stopped = "-+"
renderArt Accent = "->"

renderInStaff :: [InTime (Note MultiPitchLy)] -> String
renderInStaff mus = concat $ intersperse " " $ map f mus where
    f note = note
        & startRenderingNote
        & renderNoteBodyInStaff
        & renderNoteItems
        & extractRenderedNote

extractRenderedNote :: NoteInProgress -> String
extractRenderedNote (n,w) = 
    (concat $ w^.preceeding)
    ++ (w^.body)
    ++ (renderDuration $ w^.duration)
    ++ (concat $ w^.noteItems)
    ++ (concat $ reverse $ w^.following)

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

renderPitch' :: Pitch -> (Maybe Accidental) -> String
renderPitch' p acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)

toStage0 :: Music -> [Music]
toStage0 mus = (allVoices mus) & map (\v -> mus^.ofLine v)

allVoices :: Music -> [String]
allVoices = catMaybes.(map head).group.sort.(map (^.val.line))

isChordable :: InTime (Note Ly) -> InTime (Note Ly) -> Bool
isChordable it1 it2 = (it1^.dur == it2^.dur) && (it1^.t == it2^.t) &&
    (it1^.val.isSlurred == it2^.val.isSlurred) &&
    (it1^.val.isTied == it2^.val.isTied) &&
    (it1^.val.artics == it2^.val.artics)

formChord :: [InTime (Note Ly)] -> InTime LinearNote
formChord [it] = it & val .~ UniNote (it^.val)
formChord its = (head its) & val .~ ChordR (its & map (^.val))

combineChords :: Music -> Linear
combineChords mus = mus 
    & makeBucketsBy isChordable
    & sortBy (\a b -> ((head a)^.t) `compare` ((head b)^.t))
    & map formChord

pitchLN :: LinearNote -> Double
pitchLN (UniNote n) = pitch2num n
pitchLN (ChordR ns) = (sum $ map pitch2num ns) / (fromIntegral $ length ns)

timePitchSort :: Linear -> Linear
timePitchSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t) <> (pitchLN $ it1^.val) `compare` (pitchLN $ it2^.val)

findPolys :: Linear -> Staff
findPolys lin = reverse $ foldl f [] (timePitchSort lin) where
    f [] it = [ emptyCol & atIndex 0 .~ [it] ]
    f (current:past) it = let (voiceN,succeeded) = tryToFit it current
        in if not succeeded
           then error "too many simultaneous notes to fit on a staff"
           else if voiceN == 0
                then if all (checkLineFit it) current
                     then (emptyCol & atIndex 0 .~ [it]):current:past
                     else (current & atIndex 0 %~ (it:)):past
                else (current & atIndex voiceN %~ (it:)):past
    emptyCol = replicate Output.LilypondSettings.maxNumberOfVoices []
    tryToFit :: (InTime LinearNote) -> Polyphony -> (Int,Bool)
    tryToFit it col = tryToFitHelper $ checkFit it col
    tryToFitHelper Nothing = (undefined,False)
    tryToFitHelper (Just i) = (i,True)
    checkFit it col = snd <$> find (checkLineFit it . fst) (zip col [0..])
    checkLineFit it line = all (not . conflictsWith it) line
    conflictsWith it1 it2 = it1^.t < (it2^.t + it2^.dur) && it2^.t < (it1^.t + it1^.dur)

scoreToLy :: Stage1 -> String
scoreToLy score = basicScore (concat $ map (staffFromProgress.staffToProgress) score)

staffInstruments :: [[[NoteInProgress]]] -> [Instrument]
staffInstruments = let
    fromNoteInProgress (n,_) = case n^.pitch of
        OneLy (_,_,i) -> [i]
        ManyLy ly_a_is -> map (^._3) ly_a_is
    fromLinearInProgress = concatMap fromNoteInProgress
    fromPolyInProgress = concatMap fromLinearInProgress
    fromStaffInProgress = concatMap fromPolyInProgress
    in catMaybes . nub . fromStaffInProgress

staffToProgress :: Staff -> [[[NoteInProgress]]]
staffToProgress = map polyToProgress

staffFromProgress :: [[[NoteInProgress]]] -> String
staffFromProgress staff = basicStaff (staffInstruments staff) (concat $ intersperse " " $ map polyFromProgress staff)

polyToProgress :: Polyphony -> [[NoteInProgress]]
polyToProgress = map linToProgress

polyFromProgress :: [[NoteInProgress]] -> String
polyFromProgress [] = error "empty polyphony"
polyFromProgress [lin] = linFromProgress lin
polyFromProgress lins = " << " ++ (concat $ intersperse "\\\\" $ map (" { "++) $ map (++" } ") $ map linFromProgress lins) ++ ">> "

{-
Pack the multiple notes in a ChordR into
a single note with multiple pitches.

All the notes in a chord are theoretically supposed to be
the same except for the pitch, so we only need to look
at the first one.
-}
packChordsIntoMultiPitchNotes :: LinearNote -> (Note MultiPitchLy)
packChordsIntoMultiPitchNotes (UniNote n) = n & pitch .~ OneLy (n^.pitch,n^.acc,n^.inst)
packChordsIntoMultiPitchNotes (ChordR ns) = (head ns) & pitch .~ ManyLy (zip3 (map (^.pitch) ns) (map (^.acc) ns) (map (^.inst) ns))

linToProgress :: Linear -> [NoteInProgress]
linToProgress lin = timePitchSort lin 
    & map (& val %~ packChordsIntoMultiPitchNotes)
    & map startRenderingNote
    & map renderNoteBodyInStaff
    & map renderNoteItems

linFromProgress :: [NoteInProgress] -> String
linFromProgress lin = lin
    & map extractRenderedNote
    & intersperse " "
    & concat

allRendering :: Music -> String
allRendering mus = mus
    & toStage0
    & map combineChords
    & map timePitchSort
    & map findPolys
    & (map.map.map) reverse -- put each Linear in order
    & (map.map) (filter (not.null)) -- remove empty Linears from each Polyphony
    & scoreToLy

writeScore = putStrLn . allRendering
quickScore m = writeScore $ m & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.melody
    )