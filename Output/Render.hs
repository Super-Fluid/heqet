{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverlappingInstances #-}
module Output.Render where

import Types
import qualified Tables
import Output.Templates
import Tools
import List
import qualified Output.LilypondSettings
import LyInstances
import Meters
import qualified Instruments

import Control.Lens
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Data.Ord
import Safe
import Data.Ratio

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

-- "PRE-RENDERING"
-- fixing errors, spelling durations, stuff which always has to be done

{- If a note extends over a non-playable note,
convert it into two notes tied together 
-}
breakDurationsOverNonPlayables :: Music -> Music
breakDurationsOverNonPlayables mus = let
    nonPlayables = filter (\it -> it^.val.pitch & isPlayable) mus
    breakTimes = map (^.t) nonPlayables
    breakingFunctions = map breakDurationsAtPoint breakTimes
    in foldl (&) mus breakingFunctions -- apply all the breaking functions

breakDurationsAtPoint :: PointInTime -> Music -> Music
breakDurationsAtPoint point mus = concatMap breakNote mus where
    breakNote :: (InTime (Note Ly)) -> [(InTime (Note Ly))]
    breakNote it = let
        firstDur = point - (it^.t)
        secondDur = (it^.t) + (it^.dur) - point
        in if it^.t < point && (it^.t) + (it^.dur) > point
           then [it & dur .~ firstDur,  it & t .~ point & dur .~ secondDur]
           else [it]


-- if there's no line information, put everything on line "1"
fixLines :: Music -> Music
fixLines m = let
    lines = allLines m
    in case lines of
        [] -> assignLine "1" m
        ["all"] -> assignLine "1" m
        _ -> m

{- add a pick-up into the first measure
if appropriate. 
-}
addPartialIfNeeded :: Music -> Music
addPartialIfNeeded m = let
    firstBarline = minimumByMay (comparing (^.t)) (m^.ofType lyMeasureEventType)
    in case firstBarline of
        Nothing -> m
        Just b -> let
            startTime = getStartTime m
            duration = b^.t - startTime
            partial = InTime { 
                _val = emptyNote & pitch .~ Ly (LyPartialEvent duration) & line .~ Just "all", 
                _t = startTime,
                _dur = 0 }
            firstMeterMay = minimumByMay (comparing (^.t)) (m^.ofType lyMeterEventType)
            mWithFirstMeterAtStart = case firstMeterMay of
                Just firstMeter -> (firstMeter & t .~ startTime):
                    (filter (\it -> it^.t /= b^.t || (it^.val.pitch & typeOfLy) /= lyMeterEventType) m)
                Nothing -> m -- nothing to do
            in partial:mWithFirstMeterAtStart

-- best used when applied to a single staff worth of music
-- adds a blank instrument if none is present, so that clefs will be automatically applied
-- assigns notes missing an instrument to Unknown if there is at least one instrument
fixStaffInstruments :: Music -> Music
fixStaffInstruments m = let
    someInstruments = catMaybes $ map (^.val.inst) m
    -- we don't bother to "nub" the list since we only really care if it's null or not
    in case someInstruments of
        [] -> m & traverse.val.inst .~ Just Instruments.blank
        _ -> m & traverse.val.inst %~ (\i -> case i of 
            Nothing -> Just Instruments.unknown
            Just j -> Just j)

commonDurations :: [(Duration,String)]
commonDurations = [
     (1/4,"4")
    ,(1/2,"2")
    ,(1,"1")
    ,(2,"\\breve")
    ,(1/8,"8")
    ,(1/16,"16")
    ,(1/32,"32")
    ,(1/64,"64")
    ,(1/128,"128")
    ,(1/256,"256")
    ,(3/8,"4.")
    ,(3/4,"2.")
    ,(3/2,"1.")
    ,(3,"\\breve.")
    ,(3/16,"8.")
    ,(3/32,"16.")
    ,(3/64,"32.")
    ,(3/128,"64.")
    ,(3/256,"128.")
    ,(3/512,"256.")
     ]

renderDuration :: Duration -> String
renderDuration dur
    | isJust (lookup dur commonDurations) = fromJustNote "renderDuration" (lookup dur commonDurations)
    | dur == 0 = "1*0/1"
    | otherwise = "1*" ++ (show $ numerator dur) ++ "/" ++ (show $ denominator dur)

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
applySlursToStaff staff = staff & _2 %~ applySlursToBlankStaff

canNextPlayablePolyBeSlurredTo :: [PolyInProgress] -> [Bool]
canNextPlayablePolyBeSlurredTo [] = []
canNextPlayablePolyBeSlurredTo (p:ps) = case p of 
    (VoicesInProgress lins) -> map canLinearInProgressBeSlurredTo lins
    (StaffEventInProgress _) -> canNextPlayablePolyBeSlurredTo ps

applySlursToBlankStaff :: [PolyInProgress] -> [PolyInProgress]
applySlursToBlankStaff staff = applySlursToStaff'h [] staff where
    applySlursToStaff'h _ [] = []
    applySlursToStaff'h slurIns (poly:polys) = case poly of
        (VoicesInProgress lins) -> let
            slurableOuts = canNextPlayablePolyBeSlurredTo polys
            (slurOuts,processedPoly) = applySlursToPoly slurIns slurableOuts lins
            processedRemainder = applySlursToStaff'h slurOuts polys
            in (VoicesInProgress processedPoly) : processedRemainder
        (StaffEventInProgress _) -> poly : (applySlursToStaff'h slurIns polys)


{- 
This function basically applies slurs to each of the Linears
in the Poly. It takes slur info from before and after much like
applySlursToLinear (see below). We ensure that the given lists
are big enough for the number of Linears we have by padding the
lists we get us with False.

Note that this function actually takes a list of lins,
not a poly, as there would be no point applying slurs to 
a staff event, and that case is handled in applySlursToStaff
above.
-}
applySlursToPoly :: [Bool] -> [Bool] -> [LinearInProgress] -> ([Bool], [LinearInProgress])
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
        (slurComingFromLinear,remainder) = 
            if shouldProcessThisNote
            then applySlursToLinear'h (slurComingFromNote,[]) notes
            else applySlursToLinear'h (enteringSlur,[]) notes
        shouldProcessThisNote = any isPlayable $ map (^._1) (note^._1.pitch)
        maybeProcessedNote = if shouldProcessThisNote
                             then processedNote
                             else note
        in (slurComingFromLinear,maybeProcessedNote:remainder)

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
toStage0 mus = (allStaves mus) & map (\v -> (mus^.filteringBy (isOfThisLineAndSubStaff v)))

allLines :: Music -> [String]
allLines = catMaybes.nub.sort.(map (^.val.line))

-- all staves will split the music into multiple staves for instruments like piano
allStaves :: Music -> [(String,Maybe SubStaff)]
allStaves m = let
    rough :: [(Maybe String, Maybe SubStaff)]
    rough = map (\x -> (x^.val.line,x^.val.subStaff)) m
    cleaned = filter (\(s, _) -> isJust s) rough
    specific = filter (\(s, _) -> s /= Just "all") cleaned -- "all" is not a line by itself
    in specific & traverse._1 %~ fromJust & nub

isOfThisLineAndSubStaff :: (String,Maybe SubStaff) -> LyNote -> Bool
isOfThisLineAndSubStaff (s,ss) n = 
    (n^.val.line == Just s || n^.val.line == Just "all")
    && 
    (n^.val.subStaff == ss)

-- Inserts rests into a STAFF of music where there are gaps
insertRests :: Music -> Music
insertRests m = let 
    ms = m^.annotatedMeasures
    in concatMap insertRestsIntoMeasure ms

insertRestsIntoMeasure :: (PointInTime,Music,PointInTime) -> Music
insertRestsIntoMeasure (barStart,bar,barEnd) = let
    playableNotes = traceShow (barStart,barEnd) $ bar^.playables -- trace ('\n':(show (barStart,bar^..traverse.val.pitch,barEnd))) $
    otherNotation = bar^.notPlayables
    sorted = sortBy (comparing (^.t)) playableNotes
    noOldRests = filter (\it -> (it^.val.pitch & typeOfLy) /= lyRestType) sorted
    f :: Music -> LyNote -> Music
    f [] it = 
        if it^.t == barStart 
        then [it]
        else [it,    it & t .~ barStart & dur .~ (it^.t - barStart) & val.pitch .~ Ly LyRest ]
    f (recent:past) it
        | it^.t == prevEndTime   = it:recent:past -- notes line up perfectly
        | it^.t > prevEndTime    = it:newRest:recent:past  -- gap
        | otherwise   = it:recent:past  -- overlap
            where
            newRest = it 
                & t .~ prevEndTime 
                & dur .~ (it^.t - prevEndTime) 
                & val.pitch .~ Ly LyRest
            prevEndTime = recent^.t + recent^.dur
        {- We don't try to fit in a rest now. Rather,
            we wait until after the notes are put into Polys
            and process each voice of the Poly separately -}
    mostOfProcessedBar = foldl f [] noOldRests
    finishingRest = case mostOfProcessedBar of
        [] -> case otherNotation of -- if there were no notes in the bar
            [] -> InTime { -- if there was nothing at all, which can only 
                            -- happen if there were no measure or partial
                            -- meaning that this is the whole piece of 
                            -- music so the music is empty...
                     _t = barStart 
                    ,_dur = barEnd - barStart
                    ,_val = emptyNote & pitch .~ Ly LyRest
                  }
            _ -> (head otherNotation) -- else there were some non-playables
                & t .~ barStart  -- so we use one of them as the template to get the line etc
                & dur .~ barEnd - barStart
                & val.pitch .~ Ly LyRest
        (last:_) -> last -- else there are notes in the bar, we want the last
                    -- which we know will be the head because the fold
                    -- reversed the ordering and it started in
                    -- chronological order.
            & t .~ (last^.t + last^.dur)
            & dur .~ barEnd - (last^.t + last^.dur)
            & val.pitch .~ Ly LyRest
    in 
        if finishingRest^.dur == 0
        then mostOfProcessedBar
        else finishingRest:mostOfProcessedBar

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
formChord its = (headNote "formChord" its) & val .~ (its & map (^.val))

combineChords :: Music -> Linear
combineChords mus = mus 
    & makeBucketsBy isChordable
    & sortBy (\a b -> ((headNote "combineChords" a)^.t) `compare` ((headNote "combineChords" b)^.t))
    & map formChord

pitchLN :: LinearNote -> Double
pitchLN ([n]) = pitch2num n
pitchLN (ns) = (sum $ map pitch2num ns) / (fromIntegral $ length ns)

timePitchSort :: Linear -> Linear
timePitchSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t) <>
                             (it1^.val & headNote "timePitchSort" & (^.pitch) & isPlayable) `compare` (it2^.val & headNote "timePitchSort" & (^.pitch) & isPlayable) <> -- non playable items come first
                             (pitchLN $ it1^.val) `compare` (pitchLN $ it2^.val)

findPolys :: Linear -> Staff
findPolys lin = reverse $ foldl f [] (timePitchSort lin) where
    f :: Staff -> (InTime LinearNote) -> Staff
    f [] it = 
        if isLinearNotePlayable (it^.val)
        then map Voices $ g [] it
        else [StaffEvent it]
    f (current:past) it = 
        if isLinearNotePlayable (it^.val)
        {- If we try to add a playable note,
            we should either start with the current Poly and try to
            fit it in, or we should make a new column if the current one
            was a StaffEvent 
        -}
        then case current of
            Voices lins -> (map Voices $ g lins it)++past
            StaffEvent e -> (map Voices $ g [] it)++[current]++past
        else (StaffEvent it):current:past
    -- g is only for fitting in playable notes
    g :: [Linear] -> (InTime LinearNote) -> [[Linear]]
    g [] it = [ emptyCol & atIndex 0 .~ [it] ]
    g current it = let (voiceN,succeeded) = tryToFit it current
        in if not succeeded
           then [current] -- discard the voices that don't fit (TODO: write error)
           else if voiceN == 0
                then if all (checkLineFit it) current
                     then (emptyCol & atIndex 0 .~ [it]):[current]
                     else [(current & atIndex 0 %~ (it:))]
                else [(current & atIndex voiceN %~ (it:))]
    emptyCol = replicate Output.LilypondSettings.maxNumberOfVoices []
    tryToFit :: (InTime LinearNote) -> [Linear] -> (Int,Bool)
    tryToFit it col = tryToFitHelper $ checkFit it col
    tryToFitHelper Nothing = (undefined,False)
    tryToFitHelper (Just i) = (i,True)
    checkFit it col = snd <$> find (checkLineFit it . fst) (zip col [0..])
    checkLineFit it line = all (not . conflictsWith it) line
    conflictsWith it1 it2 = it1^.t < (it2^.t + it2^.dur) && it2^.t < (it1^.t + it1^.dur)

isLinearNotePlayable :: LinearNote -> Bool
isLinearNotePlayable ns = any (\n -> n^.pitch & isPlayable) ns

{-
Put voices in pitch order within each poly,
for a lilypond notion of pitch order (odd on top, even on bottom,
for something like 1,3,5,2,4,6. Not sure if that's the right order
of the evens..
-}
sortPoly :: Polyphony -> Polyphony
sortPoly (Voices lins) = let
    sorted = reverse $ sortBy (comparing heightOfLinear) lins -- reverse so high to low
    indexed = sorted `zip` [1..] -- voices are 1-indexed
    odds  = map (^._1) $ filter (\(_,i) -> odd  i) indexed
    evens = map (^._1) $ filter (\(_,i) -> even i) indexed
    in Voices $ odds ++ evens
sortPoly staffEvent = staffEvent

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
scoreToLy score = basicScore (concat $ score 
    & map (placeClefChanges.
        applySlursToStaff.
        staffToProgress)
    & groupMultiStaffInstruments
    & map staffFromProgress
    )

groupMultiStaffInstruments :: ScoreInProgress -> ScoreInProgressMultiStaffInstruments
groupMultiStaffInstruments staves = let
    grouped = makeBucketsBy (\s t -> (s^._1._1) == (t^._1._1)) staves -- group by line
    sorted = map (sortBy (comparing (^._1._2))) grouped -- sort each group by subStaff
    in sorted

staffInstruments :: [PolyInProgress] -> [Instrument]
staffInstruments = let
    fromNoteInProgress (n,_) = case n^.pitch of
        [(_,_,i)] -> [i]
        ly_a_is -> map (^._3) ly_a_is
    fromLinearInProgress = concatMap fromNoteInProgress
    fromPolyInProgress (VoicesInProgress lins) = concatMap fromLinearInProgress lins
    fromPolyInProgress (StaffEventInProgress _) = []
    fromStaffInProgress = concatMap (fromPolyInProgress)
    in catMaybes . nub . fromStaffInProgress

staffToProgress :: Staff -> StaffInProgress
staffToProgress [] = ((Nothing,Nothing), []) -- should never happen I think
staffToProgress s = (((headNote "staffToProgress" s) & takeOneNote & view line ,
                      (headNote "staffToProgress" s) & takeOneNote & view subStaff), map polyToProgress s) where
    takeOneNote :: Polyphony -> Note Ly
    takeOneNote (StaffEvent it) = headNote "staffToProgress" (it^.val)
    takeOneNote (Voices its) = headNote "staffToProgress" ((head $ head its)^.val)

staffFromProgress :: [StaffInProgress] -> String
staffFromProgress [] = error "instrument with zero staves, should never happen"
staffFromProgress [staff] = basicStaff (staffInstruments (staff^._2)) (plainStaffFromProgress staff)
staffFromProgress staves = pianoStaff (staffInstruments (concatMap (^._2) staves)) (map plainStaffFromProgress staves)

plainStaffFromProgress :: StaffInProgress -> String
plainStaffFromProgress staff = concat $ intersperse " " $ map polyFromProgress (staff^._2)

polyToProgress :: Polyphony -> PolyInProgress
polyToProgress (Voices lins) = VoicesInProgress $ map linToProgress lins
polyToProgress (StaffEvent e) = StaffEventInProgress e' where
    lys = e^..val.traverse.pitch
    multi :: MultiPitchLy
    multi = map (\ly -> (ly,Nothing,Nothing)) lys
    eWithMulti :: InTime (Note MultiPitchLy)
    eWithMulti = e & val .~ (headNote "polyToProgress" (e^.val) & pitch .~ multi)
    e' = startRenderingNote eWithMulti

polyFromProgress :: PolyInProgress -> String
polyFromProgress (VoicesInProgress []) = error "empty polyphony"
polyFromProgress (VoicesInProgress [lin]) = linFromProgress lin
polyFromProgress (VoicesInProgress lins) = " << " ++ (concat $ intersperse "\\\\" $ map (" { "++) $ map (++" } ") $ map linFromProgress lins) ++ ">> "
polyFromProgress (StaffEventInProgress e) = extractStaffEvent e

extractStaffEvent :: NoteInProgress -> String
extractStaffEvent (n, _) = let 
    multi = n^.pitch 
    in 
        if (typeOfLy ((^._1) $ headNote "extractStaffEvent" $ n^.pitch) == lyEffectType)
        then " " ++ errorRedNotehead ++ " " ++ (xNote n)
        else case multi of
                [] -> "" -- should not happen
                xs -> concat $ intersperse " " $ map (\(Ly a,_,_) -> renderInStaff n a) xs
        --  there should only be one though, map just to be careful

placeClefChanges :: StaffInProgress -> StaffInProgress
placeClefChanges s = s & _2 %~ placeClefChanges'h Nothing where
    placeClefChanges'h :: (Maybe Clef) -> [PolyInProgress] -> [PolyInProgress]
    placeClefChanges'h _ [] = []
    placeClefChanges'h c ((StaffEventInProgress e):polys) = 
        (StaffEventInProgress e):(placeClefChanges'h c polys)
    placeClefChanges'h c (poly@(VoicesInProgress lins):polys) = let
        changeClef :: Clef -> PolyInProgress
        changeClef c = (StaffEventInProgress ((emptyNote & pitch .~ [(Ly (LyClefEvent c),Nothing,Nothing)]), emptyWrittenNote))
        newClef = (headNote "placeClefChanges 2".headNote "placeClefChanges 1" $ lins) ^._1.clef 
            -- poly should have >=1 voice, lin should have >=1 notes
        in case newClef of
            Nothing -> poly:(placeClefChanges'h c polys) -- but every note should have a clef...
            Just new -> case c of
                Nothing -> (changeClef new):poly:(placeClefChanges'h (Just new) polys)
                Just old -> if old == new
                            then poly:(placeClefChanges'h c polys)
                            else (changeClef new):poly:(placeClefChanges'h (Just new) polys)

placeMeterChanges :: Music -> Music
placeMeterChanges m = let
    measuresAndBeats :: Music
    measuresAndBeats = m^.filteringBy (\it -> 
        (it^.val.pitch & typeOfLy) == lyMeasureEventType ||  
        (it^.val.pitch & typeOfLy) == lyBeatEventType)
        & sortBy (comparing (\it -> it^.t) <> 
            comparing (\it -> (it^.val.pitch & typeOfLy) == (typeOf LyBeatEvent)))
        -- sort by time, with a measure before its first beat
    notMeasuresAndBeats = m^.filteringBy (\it -> 
        (it^.val.pitch & typeOfLy) /= lyMeasureEventType && 
        (it^.val.pitch & typeOfLy) /= lyBeatEventType)
    takeMeasures :: [Music] -> LyNote -> [Music]
    takeMeasures acc it = 
        if (typeOfLy $ it^.val.pitch) == lyMeasureEventType
        then [it]:acc -- new measure
        else case acc of -- add beats
            [] -> [[it]]
            (a:acc) -> (it:a):acc
    segmentedMeasuresAndBeats :: [Music]
    segmentedMeasuresAndBeats = reverse $ (map reverse) $ foldl takeMeasures [] measuresAndBeats
    segmentedBeats = segmentedMeasuresAndBeats & map (^.ofType lyBeatEventType)
    -- reverse puts the music and measures back in chronological order
    beatParts = map (^..traverse.t) segmentedBeats
    beatPartsFrom0 = zipWith (\bs pit -> map (subtract pit) bs) beatParts meterStartTimes
    -- measure each set of beats from where the measure began
    -- the first beat of each list should be 0
    segmentedMeasures = segmentedMeasuresAndBeats 
        & map (^.ofType lyMeasureEventType) 
        & map (headNote "segmentedMeasures") -- we shouldn't have any measures without measure events
    meterStartTimes = segmentedMeasures^..traverse.t
    getMeasureDurations :: [LyNote] -> [Duration]
    getMeasureDurations [] = []
    getMeasureDurations [it] = [(getEndTime m) - (it^.t)]
    getMeasureDurations (this:next:more) = (next^.t - this^.t):(getMeasureDurations (next:more))
    durParts = getMeasureDurations segmentedMeasures
    signatures = traceShowId $ zip beatPartsFrom0 durParts
    maybeMeters = map (\x -> lookup x meterTable) signatures
    metersToKeep = removeAdjacentDuplicatesBy (\(a,_) (b,_) ->
        isJust a && isJust b && fromJust a == fromJust b
        ) $ zip maybeMeters meterStartTimes
    renderMeter :: ((Maybe LyMeterEvent),PointInTime) -> LyNote
    renderMeter (Nothing,pit) = InTime {
        _val = emptyNote & pitch .~ (Ly LyEffect) & errors %~ ("unknown meter":)
        ,_dur = 0
        ,_t = pit }
    renderMeter ((Just meter),pit) = InTime {
        _val = emptyNote & pitch .~ (Ly meter)
        ,_dur = 1
        ,_t = pit }
    meterChanges = map renderMeter metersToKeep
    in case notMeasuresAndBeats of
        [] -> m
        xs -> m `parI` (meterChanges & traverse.val.line .~ ((headNote "placeMeterChanges" xs)^.val.line))

{-
Pack the multiple notes in a ChordR into
a single note with multiple pitches.

All the notes in a chord are theoretically supposed to be
the same except for the pitch, so we only need to look
at the first one.
-}
packChordsIntoMultiPitchNotes :: LinearNote -> (Note MultiPitchLy)
packChordsIntoMultiPitchNotes [n] = n & pitch .~ [(n^.pitch,n^.acc,n^.inst)]
packChordsIntoMultiPitchNotes ns = (headNote "packChordsIntoMultiPitchNotes" ns) & pitch .~ (zip3 (map (^.pitch) ns) (map (^.acc) ns) (map (^.inst) ns))

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
    
{- Prepare the music for rendering -}
preRender :: Music -> Music
preRender mus = mus
    & fixLines
    & breakDurationsOverNonPlayables

allRendering :: Music -> String
allRendering mus = mus
    & preRender
    & toStage0
    & map placeMeterChanges
    & map addPartialIfNeeded
    & map fixStaffInstruments
    & map Instruments.assignAllConcertClefs
    & map insertRests
    & map combineChords
    & map timePitchSort
    & map findPolys
    & (map.map) sortPoly -- puts voices in pitch order
    & (map.map) reverseLinearsInPoly -- put each Linear in order
    & (map.map) removeEmptyLinears -- remove empty Linears from each Polyphony
    & scoreToLy

reverseLinearsInPoly :: Polyphony -> Polyphony
reverseLinearsInPoly (Voices lins) = Voices $ map reverse lins
reverseLinearsInPoly staffEvent = staffEvent

removeEmptyLinears :: Polyphony -> Polyphony
removeEmptyLinears (Voices lins) = Voices $ filter (not.null) lins
removeEmptyLinears staffEvent = staffEvent

{- 
Grace notes are like a Staff with no key changes or time signatures

We assume that all the notes in a Grace belong with this voice.
-}
allRenderingForGrace :: Note MultiPitchLy -> Music -> String
allRenderingForGrace n mus = mus
    & fixLines
    & breakDurationsOverNonPlayables
    & combineChords
    & timePitchSort
    & findPolys
    & (map) sortPoly -- puts voices in pitch order
    & (map) reverseLinearsInPoly -- put each Linear in order
    & (map) removeEmptyLinears -- remove empty Linears from each Polyphony
    & map polyToProgress
    & applySlursToBlankStaff
    & map polyFromProgress
    & intersperse " " 
    & concat