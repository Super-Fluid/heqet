{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.Render where

import Types
import qualified Tables
import Output.Templates
import Tools
import List
import qualified Output.LilypondSettings
import LyInstances

import Control.Lens
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Data.Ord
import Safe

instance Renderable LyPitch where
    renderInStaff n (LyPitch p) = renderPitchAcc (p^.pc) (n^.acc) ++ renderOct (p^.oct)
    getMarkup _ = []

instance Renderable LyRest where
    renderInStaff _ _ = "r"
    getMarkup _ = []

instance Renderable LyPerc where
    renderInStaff n _ = xNote n
    getMarkup (LyPerc s) = [markupText s]

instance Renderable LyEffect where
    renderInStaff n _ = xNote n
    getMarkup _ = []

instance Renderable LyLyric where
    renderInStaff n _ = xNote n
    getMarkup (LyLyric s) = [markupText s]

instance Renderable LyGrace where
    renderInStaff n (LyGrace mus) = "\\grace {" ++ allRenderingForGrace n mus ++ "}"
    getMarkup _ = []

instance Renderable LyMeasureEvent where
    renderInStaff _ _ = " | "
    getMarkup _ = []

instance Renderable LyKeyEvent where
    renderInStaff _ (LyKeyEvent k) = "\\key " ++ pitch ++ " " ++ mode ++ " " where
        pitch = "c"
        mode = "major"
    getMarkup _ = []

instance Renderable LyBeatEvent where
    renderInStaff _ _ = ""
    getMarkup _ = []

instance Renderable LyClefEvent where
    renderInStaff _ (LyClefEvent c) = "\\clef " ++ clef ++ " " where
        clef = case c of
            Treble -> "treble"
            Alto -> "alto"
            Tenor -> "tenor"
            Bass -> "bass"
            Treble8 -> "\"treble_8\""
            CustomClef s -> s -- let's hope the user knows what they're doing
    getMarkup _ = []

instance Renderable LyMeterEvent where
    renderInStaff _ (LyMeterEvent (Meter num denom)) = "\\time " ++ show num ++ "/" ++ show denom ++ " "
    getMarkup _ = []

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
    | dur == 0 = ""
    | otherwise = "1"

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''

startRenderingNote :: InTime (Note MultiPitchLy) -> NoteInProgress
startRenderingNote it = (it^.val, WrittenNote [] [] "" (it^.dur) [] [] False)

renderMusicErrors :: NoteInProgress -> NoteInProgress
renderMusicErrors (n,w) = let
    errorText s = "\\with-color #red { " ++ s ++ " } "
    maybeMakeRed = case n^.errors of
        [] -> id
        _ -> (errorRedNotehead:)
    markupErrors = map markupText $ map errorText (n^.errors)
    in (n, w & noteItems %~ (markupErrors++) & preceedingNoteItems %~ maybeMakeRed)

renderNoteBodyInStaff :: NoteInProgress -> NoteInProgress
renderNoteBodyInStaff (n, w) = let
    body' = case n^.pitch of 
        [ly] -> renderOneNoteBodyInStaff n ly
        lys -> renderManyNoteBodiesInStaff n lys
    nis' = case n^.pitch of
        [ly] -> (getMarkupFromOneLy ly)++(w^.noteItems)
        lys -> (getMarkupFromManyLys lys)++(w^.noteItems)
    grace = case n^.pitch of
        [x] -> isGraceNote (x^._1)
        _ -> False -- Grace notes are not auto-chordable
    in (n, w & noteItems .~ nis' & body .~ body' & graceNoteKludge .~ grace)

renderOneNoteBodyInStaff :: (Note MultiPitchLy) -> (Ly,Maybe Accidental,Maybe Instrument) -> String
renderOneNoteBodyInStaff n (Ly ly,a,_) = renderInStaff n ly

isGraceNote :: Ly -> Bool
isGraceNote (Ly a) = (typeOf a) == (typeOf (LyGrace undefined))

renderManyNoteBodiesInStaff :: (Note MultiPitchLy) -> [(Ly,Maybe Accidental,Maybe Instrument)] -> String
renderManyNoteBodiesInStaff n lys = "< " ++ (concat $ intersperse " " $ map (renderOneNoteBodyInStaff n) lys) ++ " >"

{-
Get markup to render Ly types that we don't have a better way to render.
-}
getMarkupFromOneLy :: (Ly,Maybe Accidental,Maybe Instrument) -> [String]
getMarkupFromOneLy (Ly ly,_,_) = getMarkup ly

getMarkupFromManyLys :: [(Ly,Maybe Accidental,Maybe Instrument)] -> [String]
getMarkupFromManyLys = concat . map getMarkupFromOneLy

renderNoteItems :: NoteInProgress -> NoteInProgress
renderNoteItems (n,w) = let
    beginExpr = map (^.begin) (n^.exprCommands)
    endExpr = map (^.end) (n^.exprCommands)
    beginNDExpr = map (^.begin) (n^.nonDistCommands)
    endNDExpr = map (^.end) (n^.nonDistCommands)
    articulations = map renderArt $ n^.artics
    w' = w
        & preceeding .~ beginExpr ++ beginNDExpr
        & following .~ endExpr ++ endNDExpr
        & noteItems .~ (n^.noteCommands) ++ articulations
    in (n,w')

canLinearInProgressBeSlurredTo :: LinearInProgress -> Bool
canLinearInProgressBeSlurredTo [] = False
canLinearInProgressBeSlurredTo lin = canFirstPlayableBeSlurredTo $ map (^._1.pitch) lin

-- A chord can be slurred to if any lys in it can be.
canMultiPitchLyBeSlurredTo :: MultiPitchLy -> Bool
canMultiPitchLyBeSlurredTo [(ly,_,_)] = canBeSlurredTo ly
canMultiPitchLyBeSlurredTo xs = let
    lys = map (^._1) xs
    in any canBeSlurredTo lys

canFirstPlayableBeSlurredTo :: [MultiPitchLy] -> Bool
canFirstPlayableBeSlurredTo lys = let 
    playables = filter (\mply -> any (\ly_a_is -> isPlayable (ly_a_is^._1)) mply) lys
    in case headMay playables of
        Nothing -> False
        Just p -> canMultiPitchLyBeSlurredTo p

{-
Yay Playable typeclass
-}
canBeSlurredTo :: Ly -> Bool
canBeSlurredTo (Ly a) = case info a of
    Just i -> i^.slurrable
    Nothing -> False -- we shouldn't really ask this, but let's go with False for now

{- 
We "apply" the slurs to a staff (a StaffInProgress), meaning 
that we look at the isSlurred property of notes and figure
out where to put Lilypond ( and ) marks (and sometimes tenuto
marks) to notate that articulation.
-}
applySlursToStaff :: StaffInProgress -> StaffInProgress
applySlursToStaff staff = applySlursToStaff'h [] staff where
    applySlursToStaff'h _ [] = []
    applySlursToStaff'h slurIns (poly:polys) = let
        slurableOuts = case polys of
            [] -> [] -- No more music so nothing for a slur to connect to
            (lins:_) -> map canLinearInProgressBeSlurredTo lins
        (slurOuts,processedPoly) = applySlursToPoly slurIns slurableOuts poly
        processedRemainder = applySlursToStaff'h slurOuts polys
        in processedPoly:processedRemainder

{- 
This function basically applies slurs to each of the Linears
in the Poly. It takes slur info from before and after much like
applySlursToLinear (see below). We ensure that the given lists
are big enough for the number of Linears we have by padding the
lists we get us with False.
-}
applySlursToPoly :: [Bool] -> [Bool] -> PolyInProgress -> ([Bool], PolyInProgress)
applySlursToPoly slurIns slurableOuts lins = let
    falsePad = replicate Output.LilypondSettings.maxNumberOfVoices False
    zippedData = zip3 (slurIns ++ falsePad) (slurableOuts ++ falsePad) lins
    zippedResult = map (\x -> applySlursToLinear (x^._1) (x^._2) (x^._3)) zippedData
    slurOuts = map (^._1) zippedResult
    processedLins = map (^._2) zippedResult
    in (slurOuts,processedLins)

{-
To apply slurs to the notes in a Linear (technically a
LinearInProgress) we must know whether there is a slur 
entering and whether there is something following that
can be slurred to. At the end of the process, we report
if there's a slur exiting.
-}
applySlursToLinear :: Bool -> Bool -> LinearInProgress -> (Bool, LinearInProgress)
applySlursToLinear enteringSlur existsSlurableFollowing lin =
    applySlursToLinear'h (enteringSlur,[]) lin where
    applySlursToLinear'h :: (Bool,LinearInProgress) -> LinearInProgress -> (Bool,LinearInProgress)
    applySlursToLinear'h (isInSlur,acc) [] = (isInSlur,[])
    applySlursToLinear'h (isInSlur,acc) (note:notes) = let
        existsSlurableNextNote = case notes of
            [] -> existsSlurableFollowing 
            moreNotes -> canFirstPlayableBeSlurredTo $ map (^._1.pitch) moreNotes
            -- second case: only one note remaining in
            -- this linear, so we have to use our given information about what
            -- comes after this.
        thisNoteIsSlurred = note^._1.isSlurred
        (slurComingFromNote,modifier) = applySlurToNote 
                                                isInSlur 
                                                existsSlurableNextNote 
                                                thisNoteIsSlurred
        processedNote = modifier note
        (slurComingFromLinear,remainder) = applySlursToLinear'h (slurComingFromNote,[]) notes
        in (slurComingFromLinear,processedNote:remainder)

{-
Deciding how to mark a slurs is surprisingly tricky.
Given three bits of information (is this note slurred,
is this note in a slur, is there a note after this
which this note could slur to) here's the table of
actions to take. The boolean result is whether we are
in a slur after marking this note, and the symbols, if
any, is the mark(s) to put on this note.

If this note is slurred:
slurrable next note:    T           F
                       __________________
in slur?    T          |T           F )--
            F          |T (         F --

If this note is not slurred:
slurrable next note:    T           F
                       __________________
in slur?    T          |F )         F )
            F          |F           F 
-}
applySlurToNote :: Bool -> Bool -> Bool -> (Bool,(NoteInProgress -> NoteInProgress))
applySlurToNote isInSlur existsSlurableNextNote thisNoteIsSlurred = let
    endSlur = (& _2.noteItems %~ (")":))
    beginSlur = (& _2.noteItems %~ ("(":))
    tenuto = (& _2.noteItems %~ ("--":))
            {- If a note is supposed to be slurred but there's nothing for
            it to be slurred to, we mark it as tenuto instead. There's no
            need to check if the note already has a tenuto mark since lilypond
            will ignore a duplicate. -}
    inSlurNow = existsSlurableNextNote && thisNoteIsSlurred
    mark
        | isInSlur && not thisNoteIsSlurred = endSlur
        | not isInSlur &&     existsSlurableNextNote && thisNoteIsSlurred = beginSlur
        | not isInSlur && not existsSlurableNextNote && thisNoteIsSlurred = tenuto
        |     isInSlur && not existsSlurableNextNote && thisNoteIsSlurred = tenuto.endSlur
        | otherwise = id
    in (inSlurNow,mark)

renderArt :: SimpleArticulation -> String
renderArt Staccato = "-."
renderArt Marcato = "-^"
renderArt Tenuto = "--"
renderArt Portato = "-_"
renderArt Staccatissimo = "-|"
renderArt Stopped = "-+"
renderArt Accent = "->"
{-
renderInStaff :: [InTime (Note MultiPitchLy)] -> String
renderInStaff mus = concat $ intersperse " " $ map f mus where
    f note = note
        & startRenderingNote
        & renderNoteBodyInStaff
        & renderNoteItems
        & extractRenderedNote
-}
extractRenderedNote :: NoteInProgress -> String
extractRenderedNote (n,w) = let
    nonGraceOnly :: String -> String
    nonGraceOnly s = if not (w^.graceNoteKludge) then s else ""
    in (concat $ w^.preceeding)
    ++ nonGraceOnly (concat $ w^.preceedingNoteItems)
    ++ (w^.body)
    ++ nonGraceOnly (renderDuration $ w^.duration)
    ++ nonGraceOnly (concat $ w^.noteItems)
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

toStage0 :: Music -> [Music]
toStage0 mus = (allVoices mus) & map (\v -> mus^.ofLine v)

allVoices :: Music -> [String]
allVoices = catMaybes.(map head).group.sort.(map (^.val.line))

isChordable :: InTime (Note Ly) -> InTime (Note Ly) -> Bool
isChordable it1 it2 = (it1^.dur == it2^.dur) && (it1^.t == it2^.t) &&
    (it1^.val.isSlurred == it2^.val.isSlurred) &&
    (it1^.val.isTied == it2^.val.isTied) &&
    (it1^.val.artics == it2^.val.artics) &&
    isLyChordable (it1^.val.pitch) &&
    isLyChordable (it2^.val.pitch)

isLyChordable :: Ly -> Bool
isLyChordable (Ly a) = case info a of
    Just i -> i^.chordable
    Nothing -> False -- again, we shouldn't ever use this value...

formChord :: [InTime (Note Ly)] -> InTime LinearNote
formChord [it] = it & val .~ [(it^.val)]
formChord its = (head its) & val .~ (its & map (^.val))

combineChords :: Music -> Linear
combineChords mus = mus 
    & makeBucketsBy isChordable
    & sortBy (\a b -> ((head a)^.t) `compare` ((head b)^.t))
    & map formChord

pitchLN :: LinearNote -> Double
pitchLN ([n]) = pitch2num n
pitchLN (ns) = (sum $ map pitch2num ns) / (fromIntegral $ length ns)

timePitchSort :: Linear -> Linear
timePitchSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t) <>
                             (it1^.val & head & (^.pitch) & isPlayable) `compare` (it2^.val & head & (^.pitch) & isPlayable) <> -- non playable items come first
                             (pitchLN $ it1^.val) `compare` (pitchLN $ it2^.val)

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

{-
Put voices in pitch order within each poly,
for a lilypond notion of pitch order (odd on top, even on bottom,
for something like 1,3,5,2,4,6. Not sure if that's the right order
of the evens..
-}
sortPoly :: Polyphony -> Polyphony
sortPoly lins = let
    sorted = reverse $ sortBy (comparing heightOfLinear) lins -- reverse so high to low
    indexed = sorted `zip` [1..] -- voices are 1-indexed
    odds  = map (^._1) $ filter (\(_,i) -> odd  i) indexed
    evens = map (^._1) $ filter (\(_,i) -> even i) indexed
    in odds ++ evens

{- compute the "average pitch" of
a Linear, ignoring things that don't make 
sense to include, like lyrics.
-}
heightOfLinear :: Linear -> Double
heightOfLinear lin = let
    info = map heightAndLength lin
    summedHeight = sum $ map (^._1) info
    summedDuration = sum $ map (^._2) info
    in case summedDuration of
        0 -> 0 -- we avoid div by 0, but the best result to give is not clear
        _ -> summedHeight / (fromRational summedDuration)

{-
"height" and duration of an InTime LinearNote
with the convention that a chord counts once for each
pitch and that some kinds of Lys are ignored.

The height is multiplied by the duration of this note,
which is the duration of every pitch in this note, so 
that a long high note counts more than a short one.

It may turn out better to just count the number of notes
or to count a chord as a single note...testing required.
-}
heightAndLength :: InTime LinearNote -> (Double,Rational)
heightAndLength it = let
    lys = case it^.val of
        [n] -> [n^.pitch]
        ns -> map (^.pitch) ns
    lyInfo = map heightAndLengthPossibilityOfLy lys
    summedHeight = sum $ map (^._1) lyInfo
    numberOfAverageable = length $ filter (^._2) lyInfo
    in (summedHeight*(fromRational $ it^.dur), (fromIntegral numberOfAverageable)*it^.dur)

{-
If a Ly is able to be averaged to get the "pitch" of
a phrase, we return its pitch (or other measure of height)
and True. If it's not possible, we return False and a
height of 0. The reason we must return the boolean is
so that we know not to include the duration of the note
containing this Ly in the sum of durations we use to 
calculate the average.
-}
heightAndLengthPossibilityOfLy :: Ly -> (Double,Bool)
heightAndLengthPossibilityOfLy (Ly ly) = case info ly of
    Nothing -> (0,False) -- but probably should be "error"
    Just i -> case i^.pitchHeight of
        Nothing -> (0,False)
        Just 0 -> (0,False)
        Just x -> (x,True)

scoreToLy :: Stage1 -> String
scoreToLy score = basicScore (concat $ map (staffFromProgress.applySlursToStaff.staffToProgress) score)

staffInstruments :: StaffInProgress -> [Instrument]
staffInstruments = let
    fromNoteInProgress (n,_) = case n^.pitch of
        [(_,_,i)] -> [i]
        ly_a_is -> map (^._3) ly_a_is
    fromLinearInProgress = concatMap fromNoteInProgress
    fromPolyInProgress = concatMap fromLinearInProgress
    fromStaffInProgress = concatMap fromPolyInProgress
    in catMaybes . nub . fromStaffInProgress

staffToProgress :: Staff -> StaffInProgress
staffToProgress = map polyToProgress

staffFromProgress :: StaffInProgress -> String
staffFromProgress staff = basicStaff (staffInstruments staff) (concat $ intersperse " " $ map polyFromProgress staff)

polyToProgress :: Polyphony -> PolyInProgress
polyToProgress = map linToProgress

polyFromProgress :: PolyInProgress -> String
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
packChordsIntoMultiPitchNotes [n] = n & pitch .~ [(n^.pitch,n^.acc,n^.inst)]
packChordsIntoMultiPitchNotes ns = (head ns) & pitch .~ (zip3 (map (^.pitch) ns) (map (^.acc) ns) (map (^.inst) ns))

linToProgress :: Linear -> LinearInProgress
linToProgress lin = timePitchSort lin 
    & map (& val %~ packChordsIntoMultiPitchNotes)
    & map startRenderingNote
    & map renderNoteBodyInStaff
    & map renderNoteItems
    & map renderMusicErrors

linFromProgress :: LinearInProgress -> String
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
    & (map.map) sortPoly -- puts voices in pitch order
    & (map.map.map) reverse -- put each Linear in order
    & (map.map) (filter (not.null)) -- remove empty Linears from each Polyphony
    & scoreToLy

{- 
Grace notes are like a Staff with no key changes or time signatures

We assume that all the notes in a Grace belong with this voice.
-}
allRenderingForGrace :: Note MultiPitchLy -> Music -> String
allRenderingForGrace n mus = mus
    & combineChords
    & timePitchSort
    & findPolys
    & (map) sortPoly -- puts voices in pitch order
    & (map.map) reverse -- put each Linear in order
    & (map) (filter (not.null)) -- remove empty Linears from each Polyphony
    & map polyToProgress
    & applySlursToStaff
    & map polyFromProgress
    & intersperse " " 
    & concat