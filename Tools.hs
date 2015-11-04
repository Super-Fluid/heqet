module Tools where

import Types
import List

import Control.Lens

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pt mus = fst $ foldl f ([],pt) mus where
    f (acc, pt) inT = ((inT & t .~ pt):acc, pt+(inT^.dur))

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

voice :: String -> Lens' Music Music
voice v = filteringBy (\it -> lookup "voice" (it^.val.tags) == Just v)

--takeMusic :: PointInTime -> MusicOf a -> MusicOf a
--takeMusic t mus = 

--dropMusic :: PointInTime -> MusicOf a -> MusicOf a

--reverseMusic :: MusicOf a -> MusicOf a