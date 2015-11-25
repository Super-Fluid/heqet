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

poly2 = [music| << { e2 c4} \\ { e4 c2} >> |] 
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

slur = [music| c''4.-( e''8-( g''2 c''4.-( r4 e''8-( g''2 << { e''4 e'' } \\ { c''4-( b' } >> <a' e''>1-( <c'' e''> |] & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.violin
    )-- \grace { c''16 }

badname = [music| c4 foo2 d4 |] & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.cello
    )