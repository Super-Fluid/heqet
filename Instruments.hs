{-# LANGUAGE QuasiQuotes #-}

module Instruments where

import Types
import Tools
import Input.English
import Assigners

import Control.Lens
import Data.Maybe
import Data.List

sameKind :: Instrument -> Instrument -> Bool
sameKind i1 i2 = (i1^.kind) == (i2^.kind)

sameName :: Instrument -> Instrument -> Bool
sameName i1 i2 = (i1^.name) == (i2^.name)

whatInstruments :: Music -> [Instrument]
whatInstruments m = nubBy sameKind . catMaybes $ (map (^.val.inst) m)

annotateAllPlayability :: Music -> Music
annotateAllPlayability m = let
    instruments = whatInstruments m
    functions = map annotateForInstrument instruments
    in foldl (&) m functions

-- evaluates playability for all notes
annotateAllForInstrument :: Instrument -> Music -> Music
annotateAllForInstrument i m = (i^.annotatePlayability) m

assignToInstrument :: Instrument -> Music -> Music
assignToInstrument i m = m & traverse.val.inst .~ Just i

-- evaluates playability for note already assigned for that KIND of instrument
annotateForInstrument :: Instrument -> Music -> Music
annotateForInstrument i m = annotateAllForInstrument i (m^.instKind (i^.kind))

assignAllConcertClefs :: Music -> Music
assignAllConcertClefs m = let
    instruments = whatInstruments m
    functions = map forThatInstrument instruments
    in concatMap ($m) functions

-- assigns clefs playability for all notes
assignConcertClefsAllForInstrument :: Instrument -> Music -> Music
assignConcertClefsAllForInstrument i m = (i^.assignConcertClefs) m

-- assigns clefs for note already assigned for that KIND of instrument
assignConcertClefsForInstrument :: Instrument -> Music -> Music
assignConcertClefsForInstrument i m = m & instKind (i^.kind) %~ assignConcertClefsAllForInstrument i

-- returns just the portion of music suitable for the inst, with clefs
forThatInstrument :: Instrument -> Music -> Music
forThatInstrument i m = m^.instKind (i^.kind) & (i^.assignConcertClefs)

melody = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = bassOrTreble
    , _transposition = [pp| c |]
    , _name = "Melody"
    , _shortName = "M"
    , _kind = "Abstract"
    , _nSubStaves = 1
    }

blank = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = bassOrTreble
    , _transposition = [pp| c |]
    , _name = ""
    , _shortName = ""
    , _kind = "Abstract"
    , _nSubStaves = 1
    }

unknown = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = bassOrTreble
    , _transposition = [pp| c |]
    , _name = "Unknown"
    , _shortName = "?"
    , _kind = "Abstract"
    , _nSubStaves = 1
    }

flute = Instrument { 
      _midiInstrument = "flute"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| bf |] [pp| d'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| c |]
    , _name = "Flute"
    , _shortName = "Fl"
    , _kind = "Flute"
    , _nSubStaves = 1
    }

oboe = Instrument { 
      _midiInstrument = "oboe"
    , _pickUpTime = 10
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| bf |] [pp| a'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| c |]
    , _name = "Oboe"
    , _shortName = "Ob"
    , _kind = "Oboe"
    , _nSubStaves = 1
    }

clarinet = Instrument { 
      _midiInstrument = "clarinet"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| d |] [pp| c'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| bf, |]
    , _name = "Clarinet"
    , _shortName = "Cl"
    , _kind = "Clarinet"
    , _nSubStaves = 1
    }

bassoon = Instrument { 
      _midiInstrument = "bassoon"
    , _pickUpTime = 10
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| bf,, |] [pp| f'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| c |]
    , _name = "Bassoon"
    , _shortName = "Bsn"
    , _kind = "Bassoon"
    , _nSubStaves = 1
    }

soprano_sax = Instrument { 
      _midiInstrument = "soprano sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| af |] [pp| bf''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| bf, |]
    , _name = "Soprano Sax"
    , _shortName = "S Sx"
    , _kind = "Soprano Sax"
    , _nSubStaves = 1
    }

alto_sax = Instrument { 
      _midiInstrument = "alto sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| df, |] [pp| ef'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| ef, |]
    , _name = "Alto Sax"
    , _shortName = "A Sx"
    , _kind = "Alto Sax"
    , _nSubStaves = 1
    }

tenor_sax = Instrument { 
      _midiInstrument = "tenor sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| af, |] [pp| bf'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| bf,, |]
    , _name = "Tenor Sax"
    , _shortName = "T Sx"
    , _kind = "Tenor Sax"
    , _nSubStaves = 1
    }

baritone_sax = Instrument { 
      _midiInstrument = "baritone sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| df, |] [pp| ef'' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| ef,, |]
    , _name = "Baritone Sax"
    , _shortName = "B Sx"
    , _kind = "Baritone Sax"
    , _nSubStaves = 1
    }

violin = Instrument { 
      _midiInstrument = "violin"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| g |] [pp| a'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| c |]
    , _name = "Violin"
    , _shortName = "Vln"
    , _kind = "Violin"
    , _nSubStaves = 1
    }

viola = Instrument { 
      _midiInstrument = "violin"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| c |] [pp| d'''' |]
    , _assignConcertClefs = altoAltTreble
    , _assignWrittenClefs = altoAltTreble
    , _transposition = [pp| c |]
    , _name = "Viola"
    , _shortName = "Vla"
    , _kind = "Viola"
    , _nSubStaves = 1
    }

cello = Instrument { 
      _midiInstrument = "cello"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| c, |] [pp| d''' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [pp| c |]
    , _name = "Cello"
    , _shortName = "Vc"
    , _kind = "Cello"
    , _nSubStaves = 1
    }

string_bass = Instrument { 
      _midiInstrument = "contrabass"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| e,, |] [pp| d'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [pp| c |]
    , _name = "Bass"
    , _shortName = "Bass"
    , _kind = "String Bass"
    , _nSubStaves = 1
    }

trumpet = Instrument { 
      _midiInstrument = "trumpet"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| e, |] [pp| bf'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| bf, |]
    , _name = "Trumpet"
    , _shortName = "Tpt"
    , _kind = "Trumpet"
    , _nSubStaves = 1
    }

horn = Instrument { 
      _midiInstrument = "french horn"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| b,, |] [pp| a'' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = allTreble -- TODO HORN
    , _transposition = [pp| f, |]
    , _name = "Horn"
    , _shortName = "Hn"
    , _kind = "Horn"
    , _nSubStaves = 1
    }

trombone = Instrument { 
      _midiInstrument = "trombone"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| e,, |] [pp| f'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [pp| c |]
    , _name = "Trombone"
    , _shortName = "Tbn"
    , _kind = "Trombone"
    , _nSubStaves = 1
    }

tuba = Instrument { 
      _midiInstrument = "tuba"
    , _pickUpTime = 5
    , _putDownTime = 5
    , _annotatePlayability = simpleRange [pp| d,,, |] [pp| c'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [pp| c |]
    , _name = "Tuba"
    , _shortName = "Tba"
    , _kind = "Tuba"
    , _nSubStaves = 1
    }

piano = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [pp| a,,, |] [pp| c''''' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = bassOrTreble
    , _transposition = [pp| c |]
    , _name = "Piano"
    , _shortName = "Pno"
    , _kind = "Piano"
    , _nSubStaves = 2
    }