{-# LANGUAGE QuasiQuotes #-}

module Heqet.TestCases where

import Heqet.Input.English
import Heqet.Tools
import qualified Heqet.Instruments as Instruments
import Heqet.Types
import Heqet.Meters
import Heqet.Split

import Control.Lens


linear = [music| c'4. df'8-. b2\fermata\with"^\"long\"" |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & inst .~ Just Instruments.melody
    )

chordal = [music| <c e>4. <b d fs>8-| |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & inst .~ Just Instruments.melody
    )

poly = [music| f1 << { c4 g, } \\ { e2 } >> <g, b>2 c1\fermata |] 
    & mapOverNotes (\x -> x
        & clef .~ Just Treble
        & inst .~ Just Instruments.melody
        )

poly2 = [music| << { e2 c4 r4} \\ { e4 c2 r4} >> |] 
    & mapOverNotes (\x -> x
        & clef .~ Just Treble
        & inst .~ Just Instruments.melody
        )

missingdur = [music| c1 d |] & mapOverNotes (\x -> x
    & clef .~ Just Treble
    & inst .~ Just Instruments.melody
    )

basstreble = [music| c2. d4 ef8 g c' d' ef' g' c''4 ef''4. c''16 r16 c8-> r8 r4 |] & mapOverNotes (\x -> x
    & inst .~ Just Instruments.horn
    ) 
    & Instruments.assignAllConcertClefs

slur = [music| c''4.-( e''8-( \grace { c''16 } g''2 \grace { c''16-( } c''1 c''4.-( r4 e''8-( g''2 << { e''4 e'' } \\ { c''4-( b' } >> <a' e''>1-( <c'' e''>  \grace { c''16-( d''16 } e''2 |] & mapOverNotes (\x -> x
    & inst .~ Just Instruments.violin
    )

badname = [music| c4 foo2 d4 |] & mapOverNotes (\x -> x
    & inst .~ Just Instruments.cello
    )

slurOverBar = let 
    m = [music| c''1-( d''1|]
    bar = InTime {_val = Note {_pitch = Ly LyMeasureEvent, _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing, _subStaff = Nothing}, _dur = 0, _t = 4}
    in [(m!!0),bar,(m!!1)] & mapOverNotes (\x -> x
        & inst .~ Just Instruments.violin
    )

keys = let
    a = [music| a4 b cs d|] & mapOverNotes (\x -> x & key .~ Just (A,MajorM,Just Natural))
    b = [music| ef4 f8 ef bf4 a|] & mapOverNotes (\x -> x & key .~ Just (Ds,MajorM,Just Flat))
    in (a ++ (startMusicAt 4 b)) & mapOverNotes (\x -> x
        & inst .~ Just Instruments.violin 
    )

oneNote = [music| c4 |] & traverse.val.inst .~ Just Instruments.violin

meters = let
    a = [music| c4 d4 e4 c' d' e' |] & assignMeter m3_4
    b = [music| f8 e d c b a g b |] & assignMeter m4_4
    in (a `seqI` b `seqI` a)

meters8 = let
    a = [music| c8 d e f g a |] & assignMeter m6_8
    b = [music| f8 e d c b a g b c |] & assignMeter m9_8
    in (a `seqI` b `seqI` a)

partial = [music| b,8 c8 d e f g a b c'|] & startMusicAt (-1/8) & assignMeter0 m4_4 & traverse.val.inst .~ Just Instruments.horn

fluba = let 
    a = [music| c8 d e f g a |] & traverse.val.inst .~ Just Instruments.flute
    b = [music| f8 e d c b a g b c |] & traverse.val.inst .~ Just Instruments.tuba
    in (a `seqI` b `seqI` a)

basicPiano = [music| << { c1 } \\ {c''4 d''4 e'' f''} >> c8 e g c' e' g' c'' e'' c'''1 |]
    & assignMeter m4_4
    & superBasicSplit
    & traverse.val.inst .~ Just Instruments.piano

basicPiano' = capLastMeasure basicPiano

tuningNote = [music| a'1\fermata |] & traverse.val.inst .~ Just Instruments.oboe

simpleRest = [music| c''2. r4 d''2. r8 r |] & assignMeter m4_4

moreRest = let 
    a = [music| c''2. r4 d''2. r8 r |]
    b = [music| c2. r4 d2. r8 r |]
    in (a `parI` b) & assignMeter m4_4 & superBasicSplit 