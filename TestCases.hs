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

chordal = [music| <c e>4. <b d fs>8-| |] & mapOverNotes (\x -> x
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

missingdur = [music| c1 d |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & line .~ Just "1"
    & inst .~ Just Instruments.melody
    )

basstreble = [music| c2. d4 ef8 g c' d' ef'4 g' c''2 ef''4. c''16 r8 c8-> |] & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.horn
    )