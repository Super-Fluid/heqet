module Tools where

import Types

import Control.Lens

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pt mus = fst $ foldl f ([],pt) mus where
    f (acc, pt) inT = ((inT & t .~ pt):acc, pt+(inT^.dur))

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

--takeMusic :: PointInTime -> MusicOf a -> MusicOf a
--takeMusic t mus = 

--dropMusic :: PointInTime -> MusicOf a -> MusicOf a

--reverseMusic :: MusicOf a -> MusicOf a