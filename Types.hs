{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Types where

import Control.Lens

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Show, Eq, Ord, Enum, Bounded, Read)
data RelativePitchClass = I | IIb | II | IIIb | III | IV | Vb | V | VIb | VI | VIIb | VII
    deriving (Show, Eq, Ord, Enum, Bounded, Read)
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp 
    deriving (Eq, Ord, Enum, Bounded,Show,Read)

type Octave = Int
type Duration = Rational
type PointInTime = Rational
type PerformanceDuration = Double -- in seconds
type PointInPerformance = Double -- in seconds
type NoteCommand = String
data ExprCommand = ExprCommand {
    _begin :: String
  , _end :: String
  }
  deriving (Eq,Show,Read)

data InTime a = InTime {
    _val :: a
  , _dur :: Duration
  , _t :: PointInTime
    }
    deriving (Eq,Show,Read,Functor)

type Cents = Double
data Pitch = MakePitch {
    _pc :: PitchClass 
  , _oct :: Octave 
  , _cents :: Cents
}
    deriving (Eq, Ord, Show, Read)

data Instrument = Instrument { 
      _midiInstrument :: String
    , _pickUpTime :: PerformanceDuration
    , _putDownTime :: PerformanceDuration
    , _annotatePlayability :: Music -> Music
    , _assignClefs :: Music -> Music
    , _transposition :: Pitch
    , _name :: String
    , _shortName :: String
    , _kind :: String
    }

type Lyric = String
type Perc = String -- !!! Need something better
type MusicError = String
type Dynamic = Double -- between 0 and 1
data SimpleArticulation = Marcato | Stopped | Tenuto | Staccatissimo | Accent | Staccato | Portato
    deriving (Eq, Show, Read)
data Clef = Treble | Alto | Treble8 | Tenor | Bass | CustomClef String
    deriving (Eq, Show, Read)
type Chord = (RelativePitchClass, ChordFlavor)
data ChordFlavor = MajorC | MinorC | Diminished | Augmented | MajorMinor
    deriving (Eq, Show, Read)
type Key = (PitchClass, Mode)
data Mode = MajorM | MinorM -- | MajorBlues | MinorBlues | Dorian | Lydian | etc
    deriving (Eq, Show, Read)

instance Show Instrument where
    show ins = "<Instrument " ++ (_name ins) ++ ">"

instance Eq Instrument where
    i == j = (_kind i) == (_kind j)

data Note a = Note { 
      _pitch :: a
    , _acc :: Accidental
    , _noteCommands :: [NoteCommand]
    , _exprCommands :: [ExprCommand] -- Note: head to tail == outer to inner commands
    , _nonDistCommands :: [ExprCommand]
    , _errors :: [MusicError]
    , _isSlurred :: Bool
    , _isTied :: Bool
    , _dynamic :: Maybe Dynamic
    , _artics :: [SimpleArticulation] -- all other articulations are noteCommands
    , _tags :: [(String,String)]
    , _clef :: Maybe Clef
    , _inst :: Maybe Instrument
    , _chord :: Maybe Chord
    , _key :: Maybe Key
    }
    deriving (Eq, Show)

data Ly = Pitch Pitch | Rest | Perc Perc | Effect | Lyric Lyric
    deriving (Eq,Show,Read)

type MusicOf a = [(InTime (Note a))] -- Invariant: must be sorted chronologically
type Music = MusicOf Ly

makeLenses ''InTime
makeLenses ''ExprCommand
makeLenses ''Pitch
makeLenses ''Note
makeLenses ''Instrument