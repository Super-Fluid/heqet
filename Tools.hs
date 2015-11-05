module Tools where

import Types
import List

import Control.Lens
import Data.List
import Control.Applicative
import Data.Maybe (mapMaybe)

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pt mus = fst $ foldl f ([],pt) mus where
    f (acc, pt) inT = ((inT & t .~ pt):acc, pt+(inT^.dur))

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

voice :: String -> Lens' Music Music
voice v = filteringBy (\it -> lookup "voice" (it^.val.tags) == Just v)

instName :: String -> Lens' Music Music
instName v = filteringBy (\it -> ((^.name) <$> it^.val.inst) == Just v)

instKind :: String -> Lens' Music Music
instKind v = filteringBy (\it -> ((^.kind) <$> it^.val.inst) == Just v)

timeSort :: [InTime a] -> [InTime a]
timeSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t) 

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