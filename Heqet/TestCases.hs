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

polyPiano = [music| 
<< { c1 } \\ 
{c''8. c''16 d''4 e'' f''} \\ 
{ r4 r8 g''8 bf''8 r8 r4 } >> 
c8 e g c' e' g' c'' e'' <c''' c''>1 |]
    & assignMeter m4_4
    & superBasicSplit
    & traverse.val.inst .~ Just Instruments.piano

tuningNote = [music| a'1\fermata |] & traverse.val.inst .~ Just Instruments.oboe

simpleRest = [music| c''2. r4 d''2. r8 r |] & assignMeter m4_4

moreRest = let 
    a = [music| c''2. r4 d''2. r8 r |]
    b = [music| c2. r4 d2. r8 r |]
    in (a `parI` b) & assignMeter m4_4 & superBasicSplit 

kulittaExample = [InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = F, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 2, _t = 0 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = F, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 1 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 3 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 1, _t = 1 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 2 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 9 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = B, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 5 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 11 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 3 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 3 / 4, _t = 13 / 4},InTime {_val = Note {_pitch = Ly $ LyRest, _acc = Nothing, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0 / 1, _t = 4 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 2, _t = 0 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 2, _t = 1 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = D, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 1, _t = 1 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = F, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 2 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = D, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 2, _t = 9 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 11 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = E, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 3 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 13 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = -1, _pc = B, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 7 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 15 / 4},InTime {_val = Note {_pitch = Ly $ LyRest, _acc = Nothing, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0 / 1, _t = 4 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 0 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 1 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 8, _t = 1 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 8, _t = 5 / 8},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = F, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 3 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = B, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 1, _t = 1 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 2 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = B, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 8, _t = 9 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = A, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 8, _t = 19 / 8},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 5 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = E, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 11 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 3 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = E, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 13 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 0, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 7 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 15 / 4},InTime {_val = Note {_pitch = Ly $ LyRest, _acc = Nothing, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0 / 1, _t = 4 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 1, _t = 0 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 1, _t = 1 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 2, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 2 / 1},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 9 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = G, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 5 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = C, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 3 / 4, _t = 11 / 4},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = D, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 7 / 2},InTime {_val = Note {_pitch = Ly $ LyPitch (MakePitch {_oct = 1, _pc = E, _cents = 0.0}), _acc = Just Natural, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 1 / 4, _t = 15 / 4},InTime {_val = Note {_pitch = Ly $ LyRest, _acc = Nothing, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0 / 1, _t = 4 / 1},InTime {_val = Note {_pitch = Ly $ LyRest, _acc = Nothing, _noteCommands = [], _exprCommands = [], _nonDistCommands = [], _errors = [], _isSlurred = False, _isTied = False, _dynamic = Nothing, _artics = [], _tags = [], _line = Nothing, _subStaff = Nothing, _clef = Nothing, _inst = Nothing, _chord = Nothing, _key = Nothing}, _dur = 0 / 1, _t = 0 / 1}]

kulittaExample' = kulittaExample & assignMeter m4_4 & superBasicSplit 

voiceOrder = [music| << { e''4 e'' e'' e''4 e''4 e'' e'' e''4 } \\ {e''8 r f'' r g''  r a''r b''r c''' r d''' r e''' r} >> |]