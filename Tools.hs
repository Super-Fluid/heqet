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

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pt mus = fst $ foldl f ([],pt) mus where
    f (acc, pt) inT = ((inT & t .~ pt):acc, pt+(inT^.dur))

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

assignLine :: String -> Music -> Music
assignLine s m = m & traverse.val.line .~ Just s

eraseLine :: Music -> Music
eraseLine m = m & traverse.val.line .~ Nothing

ofLine :: String -> Lens' Music Music
ofLine v = filteringBy (\it -> it^.val.line == Just v)

instName :: String -> Lens' Music Music
instName v = filteringBy (\it -> ((^.name) <$> it^.val.inst) == Just v)

instKind :: String -> Lens' Music Music
instKind v = filteringBy (\it -> ((^.kind) <$> it^.val.inst) == Just v)

timeSort :: [InTime a] -> [InTime a]
timeSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t)

getEndTime :: Music -> Duration
getEndTime its = maximum $ map (\it -> (it^.t) + (it^.dur)) (filter (\it -> it^.val.pitch & isPlayable) its)

getStartTime :: Music -> Duration
getStartTime its = minimum $ map (\it -> (it^.t)) (filter (\it -> it^.val.pitch & isPlayable) its)

{-
takeMusic :: PointInTime -> Lens' Music Music
takeMusic pit mus = lens (mapMaybe f) (\s a -> a++s) where
    f it
        | it^.t >= pit = Nothing
        | it^.t + it^.dur <= pit = Just it
        | otherwise = Just $ it & dur .~ (pit - it^.t) & val.isTied .~ True
-}
--dropMusic :: PointInTime -> MusicOf a -> MusicOf a

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

