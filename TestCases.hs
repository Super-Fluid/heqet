{-# LANGUAGE QuasiQuotes #-}

module TestCases where

import Input.English
import Tools
import qualified Instruments
import Types

import Control.Lens


linear = [music| c'4. df'8-. b2\fermata\with"^\"long\"" |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & line .~ Just "1"
    & inst .~ Just Instruments.melody
    )

chordal = [music| <c e>4. <b d fs>8-! |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & line .~ Just "1"
    & inst .~ Just Instruments.melody
    )

poly = [music| f1 << { c4 g, } \\ { e2 } >> <g, b>2 c1\fermata |] 
    & mapOverNotes (\x -> x
        & clef .~ Just Treble
        & line .~ Just "1"
        & inst .~ Just Instruments.melody
        )