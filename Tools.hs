module Tools where

import Types

import Control.Lens

mapOverNotes :: (Note a -> Note a) -> MusicOf a -> MusicOf a
mapOverNotes = map . fmap