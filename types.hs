{-# LANGUAGE TemplateHaskell #-}

module Heget.Types where

import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe)

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

data Note = Note { 
      _pitch :: Pitch
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

---- ==================-----

str2pc :: [(String,PitchClass)]
str2pc = [
    ("a",A)
   ,("b",B)
   ,("c",C)
   ,("bf",As)
    ]

pitchClass :: GenParser Char st PitchClass
pitchClass = do
    str <- many lower
    case (lookup str str2pc) of
        Nothing -> fail "oh no, bad lilypond"
        Just pc -> return pc
-- todo: throw a real parse error here! 

octUp :: GenParser Char st Char
octUp = char '\''

octDown :: GenParser Char st Char
octDown = char ','

octave = do
    os <- many1 octUp <|> many octDown
    return $ case (length os) of
        0 -> 0
        n -> n * (case (head os) of '\'' -> 1; ',' -> -1)

denom = do 
    spaces
    char '%'
    spaces
    many1 digit

noDenom :: GenParser Char st String
noDenom = do
    return "1"

maybeDenom :: GenParser Char st String
maybeDenom = try denom <|> noDenom

-- but this should NOT accept "%" as it does. \d{6%} is malformed

rationalDur :: GenParser Char st String
rationalDur = do
    string "\\d{"
    spaces
    num <- many1 digit
    denom <- maybeDenom
    spaces
    char '}'
    return $ num ++ "%" ++ denom

durBase :: GenParser Char st String
durBase = 
        many1 digit
    <|> (try $ string "\\breve")
    <|> rationalDur

durDots :: GenParser Char st [Char]
durDots = many $ char '.'

addDots :: Duration -> Int -> Duration
addDots dur numDots = dur * (2 - ((1/2) ^ numDots))

str2dur :: [(String,Duration)]
str2dur = [
    ("1",4)
   ,("2",2)
   ,("4",1)
   ,("8",1/2)
   ,("16",1/4)
   ,("32",1/8)
   ,("64",1/16)
   ,("128",1/32)
   ,("256",1/64)
   ,("\\breve",8)
   ]

duration :: GenParser Char st Duration
duration = do
    baseStr <- durBase
    base <- case (lookup baseStr str2dur) of
        Nothing -> return (read baseStr)
        Just b -> return b
    dots <- durDots
    return $ addDots base (length dots)

-- still need better error message

inputPitch :: GenParser Char st Pitch
inputPitch = do
    pc <- pitchClass
    o <- octave
    return $ Pitch { _pc = pc, _oct = o, _cents = 0 }

inputNote :: GenParser Char st (Note,Duration)
inputNote = do
    p <- inputPitch
    d <- duration
    return (Note { _pitch = p, _acc = Natural, _noteCommands = [], _exprCommands = [] }, d)
