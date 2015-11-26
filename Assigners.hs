module Assigners where

import Types
import List
import Tools
import Input.English
import LyInstances

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
eraseClefs = mapOverNotes (& clef .~ Nothing)

{- We will fold over the notes, keeping the previous Clef (actually
    Maybe Clef since we don't have to start with any knowledge), the
    time that we last changed Clef, and the accumulator of processed
    Music.  
    
    This algorithm is embarrassingly crude; I'm thinking of using a 
    Boltzmann machine eventually for better results.
    
    To create a clef assigner, supply the two clefs and the falling, 
    middle, and rising thresholds.
-}
twoClefs :: Clef -> Clef -> Pitch -> Pitch -> Pitch -> (Music -> Music)
twoClefs _ _ _ _ _ [] = []
twoClefs lowClef highClef falling middle rising its = let musicInOrder = timeSort its 
    in (^._3) $ foldl twoClefs'h (Nothing,(musicInOrder !! 1)^.t,[]) musicInOrder where
    twoClefs'h :: (Maybe Clef,PointInTime,Music) -> (InTime (Note Ly)) -> (Maybe Clef,PointInTime,Music)
    twoClefs'h (currentClef,timeOfLastChange,acc) it = case it^.val.clef of
            -- We use the given clef. If different, mark the change.
        c@(Just _) -> let 
            time = if c == currentClef then timeOfLastChange else it^.t
            in (c,time,it:acc)
            -- We must assign a clef, based on three boundaries:
                -- middle, for deciding between two clefs with no prior information 
                -- rising, the point at which we probably switch to the upper clef
                -- falling, the point at which we probably switch to the lower clef
        Nothing -> let 
            newClef = case currentClef of
                Just c 
                    | c == lowClef -> if (it^.val.pitch) `isAbove` rising &&
                        it^.t - timeOfLastChange > 1
                        then Just highClef
                        else currentClef
                    | c == highClef -> if (it^.val.pitch) `isBelow` falling &&
                        it^.t - timeOfLastChange > 1
                        then Just lowClef
                        else currentClef
                _ -> if (it^.val.pitch) `isBelow` middle 
                     then Just lowClef
                     else Just highClef
            time = if newClef == currentClef then timeOfLastChange else it^.t
            in (newClef,time,(it & val.clef .~ newClef):acc)

bassOrTreble = twoClefs Bass Treble [p| f |] [p| c' |] [p| g' |]
bassAltTenorTreble = twoClefs Bass Tenor [p| f |] [p| c' |] [p| g' |] -- TODO: threeway split
bassAltAltoTreble = twoClefs Bass Alto [p| f |] [p| c' |] [p| g' |] -- TODO: threeway split
bassAltTreble = twoClefs Bass Treble [p| c' |] [p| c'' |] [p| c'' |]
altoAltTreble = twoClefs Alto Treble [p| c' |] [p| b'' |] [p| b'' |]

-- these functions are for comparing a Ly to a Pitch, with the 
-- property that a Ly that is not a Pitch should always be 'inside'
-- the allowable range.
isAbove :: Ly -> Pitch -> Bool
isAbove (Pitch p) q = p > q
isAbove _ _ = False
isBelow :: Ly -> Pitch -> Bool
isBelow (Pitch p) q = p < q
isBelow _ _ = False

-- for defining playability:
simpleRange :: Pitch -> Pitch -> Music -> Music
simpleRange low high = mapOverNotes $ \n -> 
    if (n^.pitch) `isAbove` high
    then n & errors %~ ("too high!":)
    else if (n^.pitch) `isBelow` low
        then n & errors %~ ("too low!":)
        else n