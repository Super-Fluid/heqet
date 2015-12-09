{-# LANGUAGE TemplateHaskell
, DeriveFunctor
, ExistentialQuantification
, FlexibleInstances
, OverlappingInstances
, DeriveDataTypeable #-}

module Types where

import Control.Lens
import Data.Typeable
import Data.Maybe
import Data.Typeable

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
    , _nSubStaves :: Int
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
data SubStaff = RH | ExtraRH | LH | ExtraLH | Pedal
    deriving (Eq, Show, Read, Ord, Enum)

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
    , _subStaff :: Maybe SubStaff -- Like RH, LH, Pedal for piano, organ, etc
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
    , _subStaff = Nothing
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

{-
This is a default instance that might be overlapped by 
instances for specific types of a. So if your new type
is not playable (it does not take up time or participate
in slurring) then you don't need to declare a 
Playable instance for it.
-}
instance Playable a where
    info = const Nothing

-- this instance is not made to be used.
-- it's just so things don't complain about
-- missing Renderable instances if you don't
-- import Render, which you can't always do
-- because it can cause a cycle of imports.
instance Renderable a where
    renderInStaff _ _ = ""
    getMarkup _ = []

data PlayInfo = PlayInfo {
    _slurrable :: Bool
  , _chordable :: Bool
  , _pitchHeight :: Maybe Double -- for comparing. Not all Lys can be compared.
    }
    deriving (Show)

data LyPitch = LyPitch Pitch
    deriving (Show,Read,Typeable)
lyPitchType = typeOf (LyPitch undefined)

-- see Render for Renderable instance

data LyRest = LyRest
    deriving (Show,Read,Typeable,Eq)
lyRestType = typeOf (LyRest)

instance Renderable LyRest where
    renderInStaff _ _ = "r"
    getMarkup _ = []

data LyPerc = LyPerc Perc
    deriving (Show,Read,Typeable,Eq)
lyPercType = typeOf (LyPerc undefined)

-- see Render for Renderable instance

data LyEffect = LyEffect
    deriving (Show,Read,Typeable,Eq)
lyEffectType = typeOf (LyEffect)

-- see Render for Renderable instance

data LyLyric = LyLyric Lyric
    deriving (Show,Read,Typeable)
lyLyricType = typeOf (LyLyric undefined)

-- see Render for Renderable instance

data LyGrace = LyGrace Music
    deriving (Show,     Typeable   )
lyGraceType = typeOf (LyGrace undefined)

-- see Render for Renderable instance

data LyMeasureEvent = LyMeasureEvent
    deriving (Show,Read,Typeable,Eq)
lyMeasureEventType = typeOf (LyMeasureEvent)

instance Renderable LyMeasureEvent where
    renderInStaff _ _ = " |\n "
    getMarkup _ = []

data LyBeatEvent = LyBeatEvent
    deriving (Show,Read,Typeable,Eq)
lyBeatEventType = typeOf (LyBeatEvent)

instance Renderable LyBeatEvent where
    renderInStaff _ _ = ""
    getMarkup _ = []

data LyKeyEvent = LyKeyEvent Key
    deriving (Show,Read,Typeable,Eq)
lyKeyEventType = typeOf (LyKeyEvent undefined)

instance Renderable LyKeyEvent where
    renderInStaff _ (LyKeyEvent k) = "\\key " ++ pitch ++ " " ++ mode ++ " " where
        pitch = "c"
        mode = "major"
    getMarkup _ = []

data LyClefEvent = LyClefEvent Clef
    deriving (Show,Read,Typeable,Eq)
lyClefEventType = typeOf (LyClefEvent undefined)

instance Renderable LyClefEvent where
    renderInStaff _ (LyClefEvent c) = "\\clef " ++ clef ++ " " where
        clef = case c of
            Treble -> "treble"
            Alto -> "alto"
            Tenor -> "tenor"
            Bass -> "bass"
            Treble8 -> "\"treble_8\""
            CustomClef s -> s -- let's hope the user knows what they're doing
    getMarkup _ = []

data LyMeterEvent = LyMeterEvent Meter
    deriving (Show,Read,Typeable,Eq)
lyMeterEventType = typeOf (LyMeterEvent undefined)

instance Renderable LyMeterEvent where
    renderInStaff _ (LyMeterEvent (Meter num denom)) = "\n\\time " ++ show num ++ "/" ++ show denom ++ " "
    getMarkup _ = []


-- I actually can't think of any reason for this to exist?
data LyNull = LyNull
    deriving (Show,Read,Typeable)

instance Renderable LyNull where
    renderInStaff _ _ = ""
    getMarkup _ = []

{- just a little extra standardization when
getting the type of Ly we have.
-}
typeOfLy :: Ly -> TypeRep
typeOfLy (Ly a) = typeOf a

type MusicOf a = [(InTime (Note a))] -- Invariant: must be sorted chronologically
type Music = MusicOf Ly
type LyNote = (InTime (Note Ly)) 
-- just for clarity in type signatures
-- Music = [LyNote]

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

emptyWrittenNote = WrittenNote { 
      _preceeding = []
    , _preceedingNoteItems = []
    , _body = ""
    , _duration = 0
    , _noteItems = []
    , _following = []
    , _graceNoteKludge  = False
    }

type MultiPitchLy = [(Ly,Maybe Accidental,Maybe Instrument)]
type NoteInProgress = (Note MultiPitchLy, WrittenNote)
type LinearInProgress = [NoteInProgress]
data PolyInProgress = StaffEventInProgress NoteInProgress | VoicesInProgress [LinearInProgress]
type StaffInProgress = ((Maybe String, Maybe SubStaff),[PolyInProgress])
type ScoreInProgress = [StaffInProgress]
type ScoreInProgressMultiStaffInstruments = [[StaffInProgress]]

makeLenses ''WrittenNote
makeLenses ''InTime
makeLenses ''ExprCommand
makeLenses ''Pitch
makeLenses ''Note
makeLenses ''Instrument
makeLenses ''PlayInfo