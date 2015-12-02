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

poly2 = [music| << { e2 c4 r4} \\ { e4 c2 r4} >> |] 
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

slur = [music| c''4.-( e''8-( \grace { c''16 } g''2 \grace { c''16-( } c''1 c''4.-( r4 e''8-( g''2 << { e''4 e'' } \\ { c''4-( b' } >> <a' e''>1-( <c'' e''>  \grace { c''16-( d''16 } e''2 |] & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.violin
    )

badname = [music| c4 foo2 d4 |] & mapOverNotes (\x -> x
    & line .~ Just "1"
    & inst .~ Just Instruments.cello
    )

slurOverBar = let 
    m = [music| c''1-( d''1|]
    bar = InTime {_val = Note {_pitch = Ly LyMeasureEvent, _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0, _t = 4}
    in [(m!!0),bar,(m!!1)] & mapOverNotes (\x -> x
        & line .~ Just "1"
        & inst .~ Just Instruments.violin
    )
