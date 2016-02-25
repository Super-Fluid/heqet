{-# LANGUAGE MultiWayIf #-}

module Heqet.Output.Symbols 
    ( renderSymbols
    ) where

import Heqet.Output.Render
import Heqet.Types
import qualified Heqet.Tables as Tables
import Heqet.Output.Templates
import Heqet.Tools
import Heqet.List
import qualified Heqet.Output.LilypondSettings as Output.LilypondSettings
import Heqet.LyInstances
import Heqet.Meters
import qualified Heqet.Instruments as Instruments

import Control.Lens
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Data.Ord
import Safe
import Data.Ratio

{-
The PointInTime is where we measure the clefs and keys for the heading.
-}
renderSymbols :: Music -> StaffOrdering -> PointInTime -> ([HeadingSymbol],[Symbol])
renderSymbols m order pit = let 
    heading = [ (ClefH Treble,1) ]
    syms = concatMap toSym m
    in (heading,syms)

toSym :: InTime (Note Ly) -> [Symbol]
toSym it = let
    lyt = typeOfLy $ it^.val.pitch
    in if
        | lyt == lyPitchType -> [(NoteHead Filled,1,0,it^.t)]
        | otherwise -> []
    