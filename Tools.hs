{-# LANGUAGE FlexibleInstances, OverlappingInstances, Rank2Types #-}

module Tools where

import Types
import List

import Control.Lens
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Safe

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap

startMusicAt :: PointInTime -> MusicOf a -> MusicOf a
startMusicAt pt mus = fst $ foldl f ([],pt) mus where
    f (acc, pt) inT = ((inT & t .~ pt):acc, pt+(inT^.dur))

startMusicAtZero :: MusicOf a -> MusicOf a
startMusicAtZero = startMusicAt 0

ofLine :: String -> Lens' Music Music
ofLine v = filteringBy (\it -> it^.val.line == Just v)

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

instance Ord (InTime (Note Ly)) where
    compare it1 it2 = (it1^.t) `compare` (it2^.t) <> (pitch2num $ it1^.val) `compare` (pitch2num $ it2^.val)

instance (Eq a) => Ord (InTime a) where
    compare it1 it2 = (it1^.t) `compare` (it2^.t)

pitch2num :: Note Ly -> Double
pitch2num n = let x = ly2num (n^.pitch)
    in  if isJust x
        then fromJust x
        else fromJust $ (lookup "verse" (n^.tags) >>= readMay >>= return.(0-)) <|> Just 0

ly2num :: Ly -> Maybe Double
ly2num (Pitch p) = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
ly2num Rest = Just 0
ly2num (Perc _) = Just 0 -- expand on this according to common drum notation?
ly2num (Lyric _) = Nothing
ly2num (Grace _) = Just 0 

durationOf :: MusicOf a -> Duration
durationOf m = max $ map (\it -> it^.t + it^.dur) m