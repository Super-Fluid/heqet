{-# LANGUAGE TemplateHaskell
, DeriveFunctor
, ExistentialQuantification
, DeriveDataTypeable #-}

module Types where

import Control.Lens
import Data.Typeable
import Data.Maybe

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
    _oct :: Octave
  , _pc :: PitchClass 
  , _cents :: Cents
}
    deriving (Eq, Ord, Show, Read)

data Instrument = Instrument { 
      _midiInstrument :: String
    , _pickUpTime :: PerformanceDuration
    , _putDownTime :: PerformanceDuration
    -- in concert pitch
    , _annotatePlayability :: Music -> Music
    , _assignConcertClefs :: Music -> Music
    , _assignWrittenClefs :: Music -> Music
    -- the pitch that the instrument sounds when playing a written middle c
    , _transposition :: Pitch
    , _name :: String
    , _shortName :: String
    -- name can be whatever, like "Siouxza" (the YPMB way to write "Sousaphone"),
    -- but the kind should be standard, like "Tuba" in this case.
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
type Key = (PitchClass, Mode, Maybe Accidental)
data Mode = MajorM | MinorM -- | MajorBlues | MinorBlues | Dorian | Lydian | etc
    deriving (Eq, Show, Read)

data Meter = Meter Int Int
    deriving (Show,Read,Eq)
data Meter' = Subs [SubMeter]
    deriving (Show,Read)
data SubMeter = SubMeter [Int] Int
    deriving (Show,Read)

instance Show Instrument where
    show ins = "<Instrument " ++ (_name ins) ++ ">"

instance Eq Instrument where
    i == j = (_name i) == (_name j)

data Note a = Note { 
      _pitch :: a
    , _acc :: Maybe Accidental
    , _noteCommands :: [NoteCommand]
    , _exprCommands :: [ExprCommand] -- Note: head to tail == outer to inner commands
    , _nonDistCommands :: [ExprCommand]
    , _errors :: [MusicError]
    , _isSlurred :: Bool
    , _isTied :: Bool
    , _dynamic :: Maybe Dynamic
    , _artics :: [SimpleArticulation] -- all other articulations are noteCommands
    , _tags :: [(String,String)]
    , _line :: Maybe String
    , _clef :: Maybe Clef
    , _inst :: Maybe Instrument
    , _chord :: Maybe Chord
    , _key :: Maybe Key
    }
    deriving (Eq, Show)

emptyNote :: Note Ly
emptyNote = Note {
      _pitch = Ly LyNull
    , _acc = Nothing
    , _noteCommands = []
    , _exprCommands = []
    , _nonDistCommands = []
    , _errors = []
    , _isSlurred = False
    , _isTied = False
    , _dynamic = Nothing
    , _artics = []
    , _tags = []
    , _line = Nothing
    , _clef = Nothing
    , _inst = Nothing
    , _chord = Nothing
    , _key = Nothing
    }

data Ly = forall a. (Renderable a, Playable a, Typeable a, Show a) => Ly a
    deriving (Typeable)

isPlayable :: Ly -> Bool
isPlayable (Ly a) = isJust $ info a

instance Show Ly where
    show (Ly a) = "Ly " ++ show a

class Renderable a where
    renderInStaff :: (Note MultiPitchLy) -> a -> String
    getMarkup :: a -> [String]

class Playable a where
    info :: a -> Maybe PlayInfo
    info = const Nothing
    -- What's playable? Notes are, key changes aren't.
    -- Can a slur pass through it? If so, then it's not playable.

    -- "info" is a Maybe because not all things are playable. Now you ask,
    -- shouldn't things that aren't playable just not be instances of
    -- this class? Well, because I want a list of things, some
    -- of which are playable and some which aren't, with the ability
    -- to filter out just the playable ones on the occasion that we
    -- need playable things. Unfortunately, I can't figure out a way to
    -- filter things by class; there's no "Classable" like there is Typeable.

data PlayInfo = PlayInfo {
    _slurrable :: Bool
  , _chordable :: Bool
  , _pitchHeight :: Maybe Double -- for comparing. Not all Lys can be compared.
    }
    deriving (Show)

data LyPitch = LyPitch Pitch
    deriving (Show,Read,Typeable)
data LyRest = LyRest
    deriving (Show,Read,Typeable)
data LyPerc = LyPerc Perc
    deriving (Show,Read,Typeable)
data LyEffect = LyEffect
    deriving (Show,Read,Typeable)
data LyLyric = LyLyric Lyric
    deriving (Show,Read,Typeable)
data LyGrace = LyGrace Music
    deriving (Show,     Typeable)
data LyMeasureEvent = LyMeasureEvent
    deriving (Show,Read,Typeable)
data LyBeatEvent = LyBeatEvent
    deriving (Show,Read,Typeable)
data LyKeyEvent = LyKeyEvent Key
    deriving (Show,Read,Typeable)
data LyClefEvent = LyClefEvent Clef
    deriving (Show,Read,Typeable)
data LyMeterEvent = LyMeterEvent Meter
    deriving (Show,Read,Typeable)
data LyNull = LyNull
    deriving (Show,Read,Typeable)

instance Renderable LyNull where
    renderInStaff _ _ = ""
    getMarkup _ = []

instance Playable LyNull where
    info _ = Nothing

type MusicOf a = [(InTime (Note a))] -- Invariant: must be sorted chronologically
type Music = MusicOf Ly

type Beat = Rational
type Measure = [Beat]
type MeasureTrack = [Measure]

type Tempo = (PointInTime -> PointInPerformance)

-- Types for rendering

type LinearNote = [Note Ly]
type Linear = [InTime LinearNote]
data Polyphony = StaffEvent (InTime LinearNote) | Voices [Linear]
    deriving (Show)
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

type MultiPitchLy = [(Ly,Maybe Accidental,Maybe Instrument)]
type NoteInProgress = (Note MultiPitchLy, WrittenNote)
type LinearInProgress = [NoteInProgress]
data PolyInProgress = StaffEventInProgress NoteInProgress | VoicesInProgress [LinearInProgress]
type StaffInProgress = [PolyInProgress]
type ScoreInProgress = [StaffInProgress]

makeLenses ''WrittenNote
makeLenses ''InTime
makeLenses ''ExprCommand
makeLenses ''Pitch
makeLenses ''Note
makeLenses ''Instrument
makeLenses ''PlayInfo