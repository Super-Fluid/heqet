{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens


data Music a = 
    SingleNote Duration a | 
    Par (Music a) (Music a) | 
    Seq (Music a) (Music a) |
    Error String (Music a) | 
    CommandMusicExpression String (Music a) |
    CommandStartStop String String (Music a) | 
    CommandPrevNote String a | 
    CommandNextNote String a
    
    deriving (Show, Eq,Read)

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum, Bounded, Read)
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Eq, Ord, Enum, Bounded,Show,Read)

type Octave = Int
type Duration = Rational
type PointInTime = Rational
type NoteCommand = String
data ExprCommand = ExprCommand {
    _begin :: String
  , _end :: String
  }
  deriving (Eq,Show,Read)
makeLenses ''ExprCommand

data InTime a = InTime {
    _val :: a
  , _dur :: Duration
  , _t :: PointInTime
    }
    deriving (Eq,Show,Read)
makeLenses ''InTime

type Music' a = [(InTime a)] -- Invariant: must be sorted chronologically

type Cents = Double
data Pitch = Pitch {
    _pc :: PitchClass 
  , _oct :: Octave 
  , _cents :: Cents
}
    deriving (Eq, Ord, Show, Read)
makeLenses ''Pitch

type Lyric = String
type Perc = Pitch

data Pitch' = RegPitch Pitch | Rest | Perc Perc
    deriving (Eq,Show,Read)

data Note = Note { 
      _pitch :: Pitch'
    , _acc :: Accidental
    , _noteCommands :: [NoteCommand]
    , _exprCommands :: [ExprCommand] -- Note: head to tail == outer to inner commands
    }
    deriving (Eq, Show, Read)
makeLenses ''Note

data Instrument = Instrument { 
      _midiInstrument :: String
--  , available_notes :: Either range set
--  , pick_up_time :: ?
--  , put_down_time :: ?
    , _annotatePlayability :: (Music Note) -> (Music Note)
    , _assignClefs :: (Music Note) -> (Music Note)
    , _transposition :: Pitch
    , _name :: String
    , _shortName :: String
    , _kind :: String
    }
makeLenses ''Instrument

instance Show Instrument where
    show ins = "<Instrument " ++ ins^.name ++ ">"

instance Eq Instrument where
    i == j = i^.kind == j^.kind

type Instruments = [Instrument]

data StaffName = Auto | Manual String
    deriving (Show, Eq, Read)
data Voice = Voice (Music Note)
    deriving (Show, Eq, Read)
data StaffType = DrumStaff | TabStaff | CommonStaff
    deriving (Show, Eq, Read)
data Staff = Staff { _staffType :: StaffType
                   , _voices :: [Voice]
                   , _inss :: Instruments
                   , _staffName :: StaffName
                   }
    deriving (Show)
makeLenses ''Staff
data StaffOrStaffGroup = SingleStaff Staff | GroupOfStaves StaffGroup
    deriving (Show)
data StaffGroupType = ChoirStaff | GrandStaff | PlainGroup | PianoStaff
    deriving (Show, Eq)
data StaffGroup = StaffGroup { _staffGroupType :: StaffGroupType
                             , _staves :: [StaffOrStaffGroup]
                             , _staffGroupName :: StaffName 
                             }
    deriving (Show)
makeLenses ''StaffGroup
type Header = String
data BookPart = Markup String | Score Header StaffOrStaffGroup
    deriving (Show)
type Book = [BookPart]

----

data Transpositionality = Concert | TruePitch | Transposed
-- Concert scores still transpose some instruments by octaves (bass, piccolo, celeste, guitar)

type Piece = [(Music' Note)]