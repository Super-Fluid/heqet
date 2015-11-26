{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.RenderTypes where

import Types

import Control.Lens

type ChordR = [Note Ly]
data LinearNote = ChordR ChordR | UniNote (Note Ly)
    deriving (Show)
type Linear = [InTime LinearNote]
type Polyphony = [Linear]
type Staff = [Polyphony]
type Stage1 = [Staff]

data WrittenNote = WrittenNote { 
      _preceeding :: [String]
    , _preceedingNoteItems :: [String]
    , _body :: String
    , _duration :: Duration
    , _noteItems :: [String]
    , _following :: [String]
    , _graceNoteKludge :: Bool -- if true, don't write any duration or noteItems
    }
    deriving (Show)
makeLenses ''WrittenNote

data MultiPitchLy = OneLy (Ly,Maybe Accidental,Maybe Instrument) 
    | ManyLy [(Ly,Maybe Accidental,Maybe Instrument)]
    deriving (Show)

type NoteInProgress = (Note MultiPitchLy, WrittenNote)
type LinearInProgress = [NoteInProgress]
type PolyInProgress = [LinearInProgress]
type StaffInProgress = [PolyInProgress]
type ScoreInProgress = [StaffInProgress]