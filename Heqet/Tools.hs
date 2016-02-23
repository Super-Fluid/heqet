{-# LANGUAGE FlexibleInstances, OverlappingInstances, Rank2Types #-}

module Heqet.Tools where

import Heqet.Types
import Heqet.List
import Heqet.LyInstances

import Control.Lens
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Ord
import Safe
import Data.Typeable

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes f m = m & content.traverse.val %~ f

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pit mus = let
    currentStartTime = mus ^. beginTime
    shift = pit - currentStartTime
    in mus 
        & content.traverse.t %~ (+shift)
        & beginTime .~ pit
        & endTime %~ (+shift)

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

startingMusicAt :: PointInTime -> Lens' (MusicOf a) (MusicOf a)
startingMusicAt pit = let 
    h = lens 
        (\m -> (startMusicAt pit m,m^.beginTime)) 
        (\_ (m,oldStartTime) -> startMusicAt oldStartTime m)
    in h._1 -- hide the saved oldStartTime

startingMusicAtZero :: Lens' (MusicOf a) (MusicOf a)
startingMusicAtZero = startingMusicAt 0

assignLine :: String -> Music -> Music
assignLine s m = m & content.traverse.val.line .~ Just s

eraseLine :: Music -> Music
eraseLine m = m & content.traverse.val.line .~ Nothing

musicBy :: ((InTime (Note Ly)) -> Bool) -> Lens' Music Music
musicBy p = lens (\(Mu b e s m) -> Mu b e s (filter p m)) (\(Mu b e s m)(Mu b' e' s' m') -> Mu (min b b') (max e e') Unsorted (m ++ m'))

ofLine :: String -> Lens' Music Music
ofLine v = musicBy (\it -> it^.val.line == Just v)

notOfLine :: String -> Lens' Music Music
notOfLine v = musicBy (\it -> it^.val.line /= Just v)

instName :: String -> Lens' Music Music
instName v = musicBy (\it -> ((^.name) <$> it^.val.inst) == Just v)

notInstName :: String -> Lens' Music Music
notInstName v = musicBy (\it -> ((^.name) <$> it^.val.inst) /= Just v)

instKind :: String -> Lens' Music Music
instKind v = musicBy (\it -> ((^.kind) <$> it^.val.inst) == Just v)

notInstKind :: String -> Lens' Music Music
notInstKind v = musicBy (\it -> ((^.kind) <$> it^.val.inst) /= Just v)

ofType :: TypeRep -> Lens' Music Music
ofType t = musicBy (\it -> (it^.val.pitch & typeOfLy) == t)

notOfType :: TypeRep -> Lens' Music Music
notOfType t = musicBy (\it -> (it^.val.pitch & typeOfLy) /= t)

measuresAndBeats :: Lens' Music Music
measuresAndBeats = musicBy (\it -> let t = (it^.val.pitch & typeOfLy) in 
    (t == lyMeasureEventType) || (t == lyBeatEventType)
    )

noMeasuresOrBeats :: Lens' Music Music
noMeasuresOrBeats = musicBy (\it -> let t = (it^.val.pitch & typeOfLy) in 
    (t /= lyMeasureEventType) && (t /= lyBeatEventType)
    )

playables :: Lens' Music Music
playables = musicBy (\it -> it^.val.pitch & isPlayable)

notPlayables :: Lens' Music Music
notPlayables = musicBy (\it -> it^.val.pitch & isPlayable & not)

timeSort :: [InTime a] -> [InTime a]
timeSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t)

getEndTime :: Music -> PointInTime
getEndTime m = m^.beginTime

getStartTime :: Music -> PointInTime
getStartTime m = m^.endTime

calcEndTime :: Music -> PointInTime
calcEndTime (Mu _ _ _ its) = maximumNote "getEndTime" $ map (\it -> (it^.t) + (it^.dur)) (filter (\it -> it^.val.pitch & isPlayable) its)

calcStartTime :: Music -> PointInTime
calcStartTime (Mu _ _ _ its) = minimumNote "getStartTime" $ map (\it -> (it^.t)) (filter (\it -> it^.val.pitch & isPlayable) its)

takeMusic :: PointInTime -> Lens' Music Music
takeMusic pit = 
    lens (\(Mu _ e s m) -> Mu pit (max pit e) s (takeIts pit m)) (\(Mu _ e s m)(Mu b' _ s' m') ->  Mu b' e (min s s') ((takeIts pit m')++(dropIts pit m)))

takeIts :: PointInTime -> [InTime (Note Ly)] -> [InTime (Note Ly)]
takeIts pit its = mapMaybe f its where
    f it
        | it^.t >= pit = Nothing
        | it^.t + it^.dur <= pit = Just it
        | otherwise = Just $ it & dur .~ (pit - it^.t) & val.isTied .~ True

dropMusic :: PointInTime -> Lens' Music Music
dropMusic pit =
     lens (\(Mu b _ s m) -> Mu (min pit b) pit s (dropIts pit m)) (\(Mu b _ s m)(Mu _ e' s' m') ->  Mu b e' (min s s') ((takeIts pit m)++(dropIts pit m')))


dropIts :: PointInTime -> [InTime (Note Ly)] -> [InTime (Note Ly)]
dropIts pit its = mapMaybe f its where
    f it
        | it^.t >= pit = Just it
        | it^.t + it^.dur < pit = Nothing
        | otherwise = Just $ it & dur .~ (it^.t + it^.dur - pit) & t .~ pit

sliceMusic :: PointInTime -> PointInTime -> Lens' Music Music
sliceMusic from to = (takeMusic to).(dropMusic from)

atTime :: PointInTime -> Lens' Music Music
atTime pit = musicBy (\it -> it^.t == pit)

-- This includes the partial beginning measure, if present.
-- if no measure information is present, then we'll focus on 
-- the whole music value as a single measure, so measures
-- are not guarantied to have a measure event or a partial event.
measures :: Music -> [Music]
measures = f where
    f :: Music -> [Music]
    -- our algorithm will be to take notes one by one
    -- and spawn a new measure whenever we find a 
    -- measure event
    f (Mu b e _ m) = let
        sorted = sortBy (comparing (\it -> (it^.t,it^.val.pitch 
                                            & typeOfLy 
                                            & (/= lyMeasureEventType)
                                           )
                                    )
                        ) m
        -- sort by time, with measure events coming first (because False < True)
        f'h :: [InTime (Note Ly)] -> LyNote -> [Music]
        f'h [] it = [Mu 0 0 Unsorted [it]] -- shouldn't happen?
        f'h (current@(Mu b1 e1 s1 m1):past) it = 
            if typeOfLy (it^.val.pitch) == lyMeasureEventType
            then (Mu (it^.t) ((it^.t) + (it^.dur)) Unsorted [it]):current:past
            else (Mu b1 e1 Unsorted (it:m1)):past
        in foldl f'h [(Mu b b Unsorted [])] sorted

annotatedMeasures :: Lens' Music [(PointInTime,Music,PointInTime)]
annotatedMeasures = content . lens f (\_ bars -> concat (bars^..traverse._2)) where
    f :: Music -> [(PointInTime,Music,PointInTime)]
    -- our algorithm will be to take notes one by one
    -- and spawn a new measure whenever we find a 
    -- measure event
    f m = let
        sorted = sortBy (comparing (\it -> (it^.t,it^.val.pitch 
                                            & typeOfLy 
                                            & (/= lyMeasureEventType)
                                           )
                                    )
                        ) m
        -- sort by time, with measure events coming first (because False < True)
        -- before beat events at the same times.
        f'h :: [(PointInTime,Music,PointInTime)] -> LyNote -> [(PointInTime,Music,PointInTime)]
        f'h [] it = [(it^.t,[it],it^.t + it^.dur)]
        f'h ((startT,current,currentEndT):past) it = 
            if typeOfLy (it^.val.pitch) == lyMeasureEventType
            then (it^.t,[it],it^.t):(startT,current,it^.t):past
            else (startT,it:current,it^.t + it^.dur):past
        allMeasures = foldl f'h [] sorted
        in allMeasures & filter (\(a,_,e) -> (e - a) > 0) -- remove empty 0 dur measure at end

-- split by notes with meet the predicate, so each
-- segment starts with one such note, except for
-- the first segment
segmentedBy :: (LyNote -> Bool) -> Lens' Music [Music]
segmentedBy pred = content . lens f (\_ bars -> concat bars) where
    f :: Music -> [Music]
    -- our algorithm will be to take notes one by one
    -- and spawn a new measure whenever we find a 
    -- measure event
    f m = let
        sorted = sortBy (comparing (^.t)) m
        -- sort by time, with measure events coming first (because False < True)
        f'h :: [Music] -> LyNote -> [Music]
        f'h [] it = [[it]]
        f'h (current:past) it = 
            if pred it
            then [it]:current:past
            else (it:current):past
        in foldl f'h [] sorted

--reverseMusic :: MusicOf a -> MusicOf a
{-
instance Ord (InTime (Note Ly)) where
    compare it1 it2 = (it1^.t) `compare` (it2^.t) <> (pitch2num $ it1^.val) `compare` (pitch2num $ it2^.val)
-}
instance (Eq a) => Ord (InTime a) where
    compare it1 it2 = (it1^.t) `compare` (it2^.t)

pitch2num :: Note Ly -> Double
pitch2num n = let ly = n^.pitch
                  x = Just 0 -- .info._Just.pitchHeight
    in  if isJust x
        then fromJust x
        else fromJust $ (lookup "verse" (n^.tags) >>= readMay >>= return.(0-)) <|> Just 0
{- 
ly2num :: Ly -> Maybe Double
ly2num (Pitch p) = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
ly2num Rest = Just 0
ly2num (Perc _) = Just 0 -- expand on this according to common drum notation?
ly2num (Lyric _) = Nothing
ly2num (Grace _) = Just 0 
ly2num _ = Nothing
-}

transpose :: Pitch -> Music -> Music
transpose _ = id -- TODO!!

firstNote :: Lens' Music Music
firstNote = content . lens (\its -> let
	    playables = (filter (\it -> it^.val.pitch & isPlayable) its)
	    first = minimumBy (comparing (^.t)) playables
	  in case playables of
	     [] -> []
	     _ -> [first]
	  ) (parI) 

applyDynamic :: Dynamic -> Music -> Music
applyDynamic dyn m = m & content.traverse.val.dynamic .~ Just dyn

applyArt :: SimpleArticulation -> Music -> Music
applyArt art m = m & content.traverse.val.artics %~ (art:)

applyNoteCommand :: NoteCommand -> Music -> Music
applyNoteCommand ni = (& content.firstNote.traverse.val.noteCommands %~ (appendNI ni))

applyNoteCommandToAll :: NoteCommand -> Music -> Music
applyNoteCommandToAll ni = (& content.traverse.val.noteCommands %~ (appendNI ni))

appendNI :: NoteCommand -> [NoteCommand] -> [NoteCommand]
appendNI nc ncs = if nc `elem` ncs
	    	  then ncs
		  else (nc:ncs)

-- join two pieces of Music in PARALLEL
parI :: Music -> Music -> Music
parI (Mu b1 e1 s1 m1) (Mu b2 e2 s2 m2) = Mu
    { _beginTime = min b1 b2
    , _endTime = max e1 e2
    , _sortStatus = Unsorted
    , _content = m1 ++ m2
    }

-- join two pieces of music in sequence
seqI :: Music -> Music -> Music
seqI m1 m2 = m1 `parI` (startMusicAt (getEndTime m1) m2)

-- (I stands for InTime)

getPitchHeight :: Pitch -> Maybe Double
getPitchHeight = Just . pitch2double --from LyInstances

getLyHeight :: Ly -> Maybe Double
getLyHeight (Ly a) = do
    i <- info a
    i^.pitchHeight

getNoteHeight :: LyNote -> Maybe Double
getNoteHeight it = it^.val.pitch & getLyHeight

{- 
puts a LyMeasureEvent at the end of the music,
which is important for rendering durations 
correctly. 

You should apply this function to the whole piece of music,
not individual parts.
-}
capLastMeasure :: Music -> Music
capLastMeasure mus@(Mu b e s m) = let
    cap = emptyInTime
        & t .~ e
        & val.pitch .~ Ly LyMeasureEvent
        & val.line .~ Just "all"
    in Mu b e s (cap:m)