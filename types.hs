{-# LANGUAGE TemplateHaskell #-}

module Kyuubey.Types where

import Control.Lens

data LyMusic = SingleNote LyNote| Par LyMusic LyMusic | Seq LyMusic LyMusic |
                     Error String LyMusic | CommandMusicExpression String LyMusic |
                     CommandStartStop String String LyMusic | CommandPrevNote String LyNote | CommandNextNote String LyNote
    deriving (Show, Eq)

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum, Bounded)
data Accedental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
    deriving (Show, Eq, Ord, Enum, Bounded)
type Octave = Int
type Duration = Rational
type Articulation = String

type Cents = Int
data Pitch = Pitch PitchClass Octave Cents
    deriving (Show, Eq, Ord)

data LyNote = LyNote { _pc :: PitchClass -- use Pitch type ???????
                     , _acc :: Accedental
                     , _cents :: Cents
                     , _oct :: Octave 
                     , _dur :: Duration
                     , _arts :: [Articulation]
                     }
    deriving (Show, Eq)
makeLenses ''LyNote

data Instrument = Instrument { _midiInstrument :: String
--                             , available_notes :: Either range set
--                             , pick_up_time :: ?
--                             , put_down_time :: ?
                             , _clefAssigner :: LyMusic -> LyMusic
                             , _transposition :: Pitch
                             , _name :: String
                             , _shortName :: String
                             }
makeLenses ''Instrument

instance Show Instrument where
    show i = "Instrument: " ++ i^.name

type Instruments = [Instrument]

data StaffName = Auto | Manual String
    deriving (Show, Eq)

data Voice = Voice LyMusic
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


