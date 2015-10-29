{-# LANGUAGE QuasiQuotes #-}

module Instruments where

import Types
import Tools
import Input.English

import Control.Lens

melody = Instrument { 
      _midiInstrument = "oboe"
    , _pickUpTime = 0
    , _putDownTime = 0
    , _annotatePlayability = id
    , _assignClefs = mapOverNotes (& clef .~ Just Treble)
    , _transposition = [p| c |]
    , _name = "Melody"
    , _shortName = "M"
    , _kind = "Abstract"
    }