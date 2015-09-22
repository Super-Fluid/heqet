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
    
    deriving (Show, Eq)

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum, Bounded)
data Accedental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Show, Eq, Ord, Enum, Bounded)
type Octave = Int
type Duration = Rational
type PointInTime = Rational
type NoteCommand = String
data ExprCommand = ExprCommand {
    _begin :: String
  , _end :: String
  }
  deriving (Eq)
makeLenses ''ExprCommand
instance Show ExprCommand where
    show cmd = "Command: \"" ++ cmd^.begin ++ " ... " ++ cmd^.end ++ "\""

data InTime a = InTime {
    _val :: a
  , _dur :: Duration
  , _t :: PointInTime
    }
    deriving (Show, Eq)
makeLenses ''InTime
    
type Music' a = [(InTime a)] -- Invariant: must be sorted chronologically

type Cents = Int
data Pitch = Pitch {
    _pc :: PitchClass 
  , _oct :: Octave 
  , _cents :: Cents
}
    deriving (Show, Eq, Ord)
makeLenses ''Pitch

type Lyric = String

data Pitch' = RegPitch Pitch | Rest | Lyric
    deriving (Show, Eq)

data Note = Note { 
      _pitch :: Pitch'
    , _acc :: Accedental
    , _noteCommands :: [NoteCommand]
    , _exprCommands :: [ExprCommand] -- Note: head to tail == outer to inner commands
    }
    deriving (Show, Eq)
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
    show i = "Instrument: " ++ i^.name
instance Eq Instrument where
    i == j = i^.kind == j^.kind

type Instruments = [Instrument]

data StaffName = Auto | Manual String
    deriving (Show, Eq)
data Voice = Voice (Music Note)
    deriving (Show, Eq)
data StaffType = DrumStaff | TabStaff | CommonStaff
    deriving (Show, Eq)
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