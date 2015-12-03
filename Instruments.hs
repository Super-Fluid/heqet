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

melody = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [pp| c |]
    , _name = "Melody"
    , _shortName = "M"
    , _kind = "Abstract"
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
    }