{-# LANGUAGE FlexibleInstances, OverlappingInstances, Rank2Types #-}

module Tools where

import Types
import List

import Control.Lens
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Ord
import Safe
import Data.Typeable
import LyInstances

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt _ [] = []
startMusicAt pit mus = let
    currentStartTime = minimumDef 0 $ (mus ^..traverse.t)
    shift = pit - currentStartTime
    in mus & traverse.t %~ (+shift)

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

startingMusicAt :: PointInTime -> Lens' (MusicOf a) (MusicOf a)
startingMusicAt pit = let 
    h = lens 
        (\m -> (startMusicAt pit m,minimumDef 0 $ (m ^..traverse.t))) 
        (\_ (m,oldStartTime) -> startMusicAt oldStartTime m)
    in h._1 -- hide the saved oldStartTime

startingMusicAtZero :: Lens' (MusicOf a) (MusicOf a)
startingMusicAtZero = startingMusicAt 0

assignLine :: String -> Music -> Music
assignLine s m = m & traverse.val.line .~ Just s

eraseLine :: Music -> Music
eraseLine m = m & traverse.val.line .~ Nothing

ofLine :: String -> Lens' Music Music
ofLine v = filteringBy (\it -> it^.val.line == Just v)

notOfLine :: String -> Lens' Music Music
notOfLine v = filteringBy (\it -> it^.val.line /= Just v)

instName :: String -> Lens' Music Music
instName v = filteringBy (\it -> ((^.name) <$> it^.val.inst) == Just v)

notInstName :: String -> Lens' Music Music
notInstName v = filteringBy (\it -> ((^.name) <$> it^.val.inst) /= Just v)

instKind :: String -> Lens' Music Music
instKind v = filteringBy (\it -> ((^.kind) <$> it^.val.inst) == Just v)

notInstKind :: String -> Lens' Music Music
notInstKind v = filteringBy (\it -> ((^.kind) <$> it^.val.inst) /= Just v)

ofType :: TypeRep -> Lens' Music Music
ofType t = filteringBy (\it -> (it^.val.pitch & typeOfLy) == t)

notOfType :: TypeRep -> Lens' Music Music
notOfType t = filteringBy (\it -> (it^.val.pitch & typeOfLy) /= t)

playables :: Lens' Music Music
playables = filteringBy (\it -> it^.val.pitch & isPlayable)

notPlayables :: Lens' Music Music
notPlayables = filteringBy (\it -> it^.val.pitch & isPlayable & not)

timeSort :: [InTime a] -> [InTime a]
timeSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t)

getEndTime :: Music -> Duration
getEndTime its = maximum $ map (\it -> (it^.t) + (it^.dur)) (filter (\it -> it^.val.pitch & isPlayable) its)

getStartTime :: Music -> Duration
getStartTime its = minimum $ map (\it -> (it^.t)) (filter (\it -> it^.val.pitch & isPlayable) its)

takeMusic :: PointInTime -> Lens' Music Music
takeMusic pit = lens (mapMaybe f) (\s a -> a++s) where
    f it
        | it^.t >= pit = Nothing
        | it^.t + it^.dur <= pit = Just it
        | otherwise = Just $ it & dur .~ (pit - it^.t) & val.isTied .~ True

dropMusic :: PointInTime -> Lens' Music Music
dropMusic pit = lens (mapMaybe f) (\s a -> a++s) where
    f it
        | it^.t >= pit = Just it
        | it^.t + it^.dur < pit = Nothing
        | otherwise = Just $ it & dur .~ (it^.t + it^.dur - pit) & t .~ pit

sliceMusic :: PointInTime -> PointInTime -> Lens' Music Music
sliceMusic from to = (takeMusic to).(dropMusic from)

-- This includes the partial beginning measure, if present.
-- if no measure information is present, then we'll focus on 
-- the whole music value as a single measure, so measures
-- are not guarantied to have a measure event or a partial event.
measures :: Lens' Music [Music]
measures = lens f (\_ bars -> concat bars) where
    f :: Music -> [Music]
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
        f'h :: [Music] -> LyNote -> [Music]
        f'h [] it = [[it]]
        f'h (current:past) it = 
            if typeOfLy (it^.val.pitch) == lyMeasureEventType
            then [it]:current:past
            else (it:current):past
        in foldl f'h [] sorted

-- split by notes with meet the predicate, so each
-- segment starts with one such note, except for
-- the first segment
segmentedBy :: (LyNote -> Bool) -> Lens' Music [Music]
segmentedBy pred = lens f (\_ bars -> concat bars) where
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
firstNote = lens (\its -> let
	    playables = (filter (\it -> it^.val.pitch & isPlayable) its)
	    first = minimumBy (comparing (^.t)) playables
	  in case playables of
	     [] -> []
	     _ -> [first]
	  ) (parI) 

applyDynamic :: Dynamic -> Music -> Music
applyDynamic dyn m = m & traverse.val.dynamic .~ Just dyn

applyArt :: SimpleArticulation -> Music -> Music
applyArt art m = m & traverse.val.artics %~ (art:)

applyNoteCommand :: NoteCommand -> Music -> Music
applyNoteCommand ni = (& firstNote.traverse.val.noteCommands %~ (appendNI ni))

applyNoteCommandToAll :: NoteCommand -> Music -> Music
applyNoteCommandToAll ni = (& traverse.val.noteCommands %~ (appendNI ni))

appendNI :: NoteCommand -> [NoteCommand] -> [NoteCommand]
appendNI nc ncs = if nc `elem` ncs
	    	  then ncs
		  else (nc:ncs)

-- join two pieces of Music in PARALLEL
-- might be revised if we reintroduce the
-- contraint that Music be in chronological order.
parI :: Music -> Music -> Music
parI = (++)

-- join two pieces of music in sequence
seqI :: Music -> Music -> Music
seqI m1 m2 = m1 ++ (startMusicAt (getEndTime m1) m2)

-- (I stands for InTime)

getPitchHeight :: Pitch -> Maybe Double
getPitchHeight = Just . pitch2double --from LyInstances

getLyHeight :: Ly -> Maybe Double
getLyHeight (Ly a) = do
    i <- info a
    i^.pitchHeight

getNoteHeight :: LyNote -> Maybe Double
getNoteHeight it = it^.val.pitch & getLyHeight
