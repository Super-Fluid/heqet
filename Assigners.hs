module Assigners where

import Types
import List
import Tools

import Control.Lens
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Safe

-- for assigning clefs
allTreble = mapOverNotes (& clef .~ Just Treble)
allAlto = mapOverNotes (& clef .~ Just Alto)
allTreble8 = mapOverNotes (& clef .~ Just Treble8)
allTenor = mapOverNotes (& clef .~ Just Tenor)
allBass = mapOverNotes (& clef .~ Just Bass)
allCustomClef s = mapOverNotes (& clef .~ Just (CustomClef s))

bassOrTreble its = (^._3) $ foldl bassOrTreble'h (Nothing,0,[]) (timeSort its) where
    bassOrTreble'h :: (Maybe Clef,PointInTime,Music) -> (InTime (Note Ly)) -> (Maybe Clef,PointInTime,Music)
    bassOrTreble'h (currentClef,timeOfLastChange,acc) it = case it^.val.clef of
        Just c -> (it^.val.clef,it^.t,it:acc)
        Nothing -> (Just Treble,it^.t,(it & val.clef .~ Just Treble):acc)
bassAltTenorTreble = allTreble
bassAltAltoTreble = allTreble
bassAltTreble = allTreble -- different from bassOrTreble in that it favors bass clef
altoAltTreble = mapOverNotes (& clef .~ Just Alto)



-- for defining playability:
simpleRange :: Pitch -> Pitch -> Music -> Music
simpleRange low hi = id -- TODO!