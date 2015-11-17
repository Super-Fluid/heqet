{-# LANGUAGE QuasiQuotes #-}

module Instruments where

import Types
import Tools
import Input.English
import Assigners

import Control.Lens

melody = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| c |]
    , _name = "Melody"
    , _shortName = "M"
    , _kind = "Abstract"
    }

flute = Instrument { 
      _midiInstrument = "flute"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| bf |] [p| d'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| c |]
    , _name = "Flute"
    , _shortName = "Fl"
    , _kind = "Flute"
    }

oboe = Instrument { 
      _midiInstrument = "oboe"
    , _pickUpTime = 10
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| bf |] [p| a'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| c |]
    , _name = "Oboe"
    , _shortName = "Ob"
    , _kind = "Oboe"
    }

clarinet = Instrument { 
      _midiInstrument = "clarinet"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| d |] [p| c'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| bf, |]
    , _name = "Clarinet"
    , _shortName = "Cl"
    , _kind = "Clarinet"
    }

bassoon = Instrument { 
      _midiInstrument = "bassoon"
    , _pickUpTime = 10
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| bf,, |] [p| f'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| c |]
    , _name = "Bassoon"
    , _shortName = "Bsn"
    , _kind = "Bassoon"
    }

soprano_sax = Instrument { 
      _midiInstrument = "soprano sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| af |] [p| bf''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| bf, |]
    , _name = "Soprano Sax"
    , _shortName = "S Sx"
    , _kind = "Soprano Sax"
    }

alto_sax = Instrument { 
      _midiInstrument = "alto sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| df, |] [p| ef'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| ef, |]
    , _name = "Alto Sax"
    , _shortName = "A Sx"
    , _kind = "Alto Sax"
    }

tenor_sax = Instrument { 
      _midiInstrument = "tenor sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| af, |] [p| bf'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| bf,, |]
    , _name = "Tenor Sax"
    , _shortName = "T Sx"
    , _kind = "Tenor Sax"
    }

baritone_sax = Instrument { 
      _midiInstrument = "baritone sax"
    , _pickUpTime = 5
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| df, |] [p| ef'' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| ef,, |]
    , _name = "Baritone Sax"
    , _shortName = "B Sx"
    , _kind = "Baritone Sax"
    }

violin = Instrument { 
      _midiInstrument = "violin"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| g |] [p| a'''' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| c |]
    , _name = "Violin"
    , _shortName = "Vln"
    , _kind = "Violin"
    }

viola = Instrument { 
      _midiInstrument = "violin"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| c |] [p| d'''' |]
    , _assignConcertClefs = altoAltTreble
    , _assignWrittenClefs = altoAltTreble
    , _transposition = [p| c |]
    , _name = "Viola"
    , _shortName = "Vla"
    , _kind = "Viola"
    }

cello = Instrument { 
      _midiInstrument = "cello"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| c, |] [p| d''' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [p| c |]
    , _name = "Cello"
    , _shortName = "Vc"
    , _kind = "Cello"
    }

string_bass = Instrument { 
      _midiInstrument = "contrabass"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| e,, |] [p| d'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [p| c |]
    , _name = "Bass"
    , _shortName = "Bass"
    , _kind = "String Bass"
    }

trumpet = Instrument { 
      _midiInstrument = "trumpet"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| e, |] [p| bf'' |]
    , _assignConcertClefs = allTreble
    , _assignWrittenClefs = allTreble
    , _transposition = [p| bf, |]
    , _name = "Trumpet"
    , _shortName = "Tpt"
    , _kind = "Trumpet"
    }

horn = Instrument { 
      _midiInstrument = "french horn"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| b,, |] [p| a'' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = allTreble -- TODO HORN
    , _transposition = [p| f, |]
    , _name = "Horn"
    , _shortName = "Hn"
    , _kind = "Horn"
    }

trombone = Instrument { 
      _midiInstrument = "trombone"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| e,, |] [p| f'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [p| c |]
    , _name = "Trombone"
    , _shortName = "Tbn"
    , _kind = "Trombone"
    }

tuba = Instrument { 
      _midiInstrument = "tuba"
    , _pickUpTime = 5
    , _putDownTime = 5
    , _annotatePlayability = simpleRange [p| d,,, |] [p| c'' |]
    , _assignConcertClefs = bassAltTenorTreble
    , _assignWrittenClefs = bassAltTenorTreble
    , _transposition = [p| c |]
    , _name = "Tuba"
    , _shortName = "Tba"
    , _kind = "Tuba"
    }

piano = Instrument { 
      _midiInstrument = "acoustic grand"
    , _pickUpTime = 3
    , _putDownTime = 3
    , _annotatePlayability = simpleRange [p| a,,, |] [p| c''''' |]
    , _assignConcertClefs = bassOrTreble
    , _assignWrittenClefs = bassOrTreble
    , _transposition = [p| c |]
    , _name = "Piano"
    , _shortName = "Pno"
    , _kind = "Piano"
    }