{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ParseExperiment where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
-- demo
import Language.Haskell.TH.Quote
import Language.Haskell.TH 
import Control.Applicative ((<*))
import Control.Lens
import Data.List (sortBy)
import Types hiding (chord)
import Tables

type PitchStr = String
type DurStr = String

data Dur1 = NoDur 
    | RationalDur Duration
    | CommonDur String Int -- will need to be looked up later
    deriving (Show)

data Pitch1 = NoteName1 String String
    | Frequency1 Double
    | Chord1 [Pitch1]
    deriving (Show)

data Pitch3 = NoteName3 String String
    | Frequency3 Double
    deriving (Show)

data Pitch5 = RegularNote PitchClass Octave Cents (Maybe Accidental)
    | Frequency5 PitchClass Octave Cents
    | Error5 String
    deriving (Show)

data NoteItem1 = Tie
    | Articulation Char
    | NoteCommand String
    | With String
    | Cents Double
    deriving (Show)

data NoteItem2 = Tie2
    | Articulation2 Char
    | NoteCommand2 String
    | LongCommand String String
    | Cents2 Double
    deriving (Show)

data TreeX p d = Function String (TreeX p d)
    | Command String String (TreeX p d)
    | Leaf p d [NoteItem1]
    | Parallel [TreeX p d]
    | Sequential [TreeX p d]
    | Grace (TreeX p d) (TreeX p d)
    deriving (Show)

data TreeY p d ni = LeafY p d [ni]
    | ParallelY [TreeY p d ni]
    | SequentialY [TreeY p d ni]
    | GraceY (TreeY p d ni) (TreeY p d ni)
    deriving (Show)

type Tree1 = TreeX Pitch1 Dur1
type Tree2 = TreeX Pitch1 Duration
type Tree3 = TreeX Pitch3 Duration
type Tree4 = TreeY Pitch3 Duration NoteItem2
type Tree5 = TreeY Pitch5 Duration NoteItem2
type Tree6 = TreeY Pitch  Duration NoteItem2

-- based on https://wiki.haskell.org/Parsing_a_simple_imperative_language

languageDef = 
    emptyDef { Token.commentStart        = "%{"
             , Token.commentEnd          = "%}"
             , Token.commentLine         = "%"
             , Token.reservedOpNames     = []
             }

lexer = Token.makeTokenParser languageDef

braces     = Token.braces     lexer
dot        = Token.dot        lexer
whiteSpace = Token.whiteSpace lexer
stringLiteral = Token.stringLiteral lexer
float = Token.float lexer
natural = Token.natural lexer
integer = Token.integer lexer

musicParser :: Parser Tree1
musicParser = tree <* eof

tree :: Parser Tree1
tree = do
    whiteSpace
    rs <- root `sepBy` whiteSpace
    whiteSpace
    return (Sequential rs)

root :: Parser Tree1
root = try function <|> try command <|> try leaf <|> voices1 <|> grace

function :: Parser Tree1
function = do
    string "\\function"
    whiteSpace
    func <- stringLiteral
    whiteSpace
    char '{'
    whiteSpace
    music <- (try tree)
    whiteSpace
    char '}'
    whiteSpace
    return $ Function func music

command :: Parser Tree1
command = do
    string "\\command"
    whiteSpace
    begin <- stringLiteral
    whiteSpace
    end <- stringLiteral
    whiteSpace
    char '{'
    whiteSpace
    music <- tree
    whiteSpace
    char '}'
    whiteSpace
    return $ Command begin end music

leaf :: Parser Tree1
leaf = do
    p <- pitch1
    dur <- duration
    whiteSpace
    items <- noteItem `sepBy` whiteSpace
    whiteSpace
    return $ Leaf p dur items

pitch1 :: Parser Pitch1
pitch1 = noteName <|> frequency <|> chord

noteName :: Parser Pitch1
noteName = do
    name <- many1 lower
    oct <- many $ oneOf "',"
    return $ NoteName1 name oct

frequency :: Parser Pitch1
frequency = do
    hz <- num
    string "hz"
    return $ Frequency1 hz

num :: Parser Double
num = try float <|> naturalFloat

naturalFloat :: Parser Double
naturalFloat = do
    n <- natural
    return $ fromIntegral n

chord :: Parser Pitch1
chord = do
    char '<'
    whiteSpace
    ps <- pitch1 `sepBy` whiteSpace
    whiteSpace
    char '>'
    return $ Chord1 ps

duration :: Parser Dur1
duration = rationalDur <|> commonDur <|> noDur

rationalDur :: Parser Dur1
rationalDur = do
    string "\\d"
    whiteSpace
    r <- rational
    return $ RationalDur r

rational :: Parser Rational
rational = do
    num <- natural
    whiteSpace
    b <- maybeDenom
    case b of
        Just denom -> return (fromIntegral num / fromIntegral denom)
        Nothing    -> return (fromIntegral num)

maybeDenom :: Parser (Maybe Integer)
maybeDenom = justDenom <|> nothingDenom

justDenom :: Parser (Maybe Integer)
justDenom = do
    char '/'
    whiteSpace
    denom <- natural
    return $ Just denom

nothingDenom :: Parser (Maybe Integer)
nothingDenom = do
    return Nothing

commonDur :: Parser Dur1
commonDur = do
    base <- many1 digit
    dots <- many $ char '.'
    return $ CommonDur base (length dots)

noDur :: Parser Dur1
noDur = return NoDur

noteItem :: Parser NoteItem1
noteItem = (tie <|> try articulation <|> try with <|> try cents1 <|> try noteCommand) <* whiteSpace

tie :: Parser NoteItem1
tie = char '~' >> return Tie

articulation :: Parser NoteItem1
articulation = do
    char '-'
    c <- oneOf "<>\\'+-!._,~/0123456789"
    return $ Articulation c


with :: Parser NoteItem1
with = do
    string "\\with"
    whiteSpace
    cmd <- stringLiteral
    whiteSpace
    return $ With cmd

cents1 :: Parser NoteItem1
cents1 = do
    string "\\cents"
    whiteSpace
    x <- pmnum
    return $ Cents x

pmnum :: Parser Double
pmnum = try pmFloat <|> integerFloat

pmFloat :: Parser Double
pmFloat = do
    minus <- optionMaybe $ char '-'
    f <- float
    case minus of
        Just _ -> return (-f)
        Nothing -> return f

integerFloat :: Parser Double
integerFloat = do
    i <- integer
    return $ fromIntegral i

noteCommand :: Parser NoteItem1
noteCommand =  do
    char '\\'
    name <- many1 alphaNum
    return $ NoteCommand name

voices1 :: Parser Tree1
voices1 = do 
    string "<<"
    whiteSpace
    vs <- (braces (try tree)) `sepBy` voicesSep
    whiteSpace
    string ">>"
    return $ Parallel vs

voicesSep :: Parser ()
voicesSep = do
    whiteSpace
    string "\\\\"
    whiteSpace
    return ()

grace :: Parser Tree1
grace = do
    string "\\grace"
    whiteSpace
    g <- braces $ tree
    whiteSpace
    n <- braces $ tree
    whiteSpace
    return $ Grace g n

--- TRANSFORMATIONS

addDots :: Duration -> Int -> Duration
addDots dur numDots = dur * (2 - ((1/2) ^ numDots))

commonDurs :: [(String,Duration)]
commonDurs = [
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
   ,("0",0)
   ]

lookupDur :: String -> Duration
lookupDur base = case (lookup base commonDurs) of
    Just d -> d
    Nothing -> error $ "unknown duration \""++base++"\". If you want an arbitrary rational duration, you need to prefix it with \"\\d\"."

makeAllDurationsRational :: Tree1 -> Tree2
makeAllDurationsRational = f where
    f (Function s mus)      = Function s (f mus)
    f (Command s t mus)     = Command s t (f mus)
    f (Leaf p d noteitems)  = Leaf p (makeDurationRational d) noteitems
    f (Parallel muss)       = Parallel (map f muss)
    f (Grace mus1 mus2)     = Grace (f mus1) (f mus2)
    f (Sequential muss)     = Sequential (map f muss)

fillInMissingDurs :: Tree1 -> Tree1
fillInMissingDurs t = fst $ f (CommonDur "4" 0) t where
    f d (Function s mus) = (Function s (fst $ f d mus), snd $ f d mus)
    f d (Command s t mus) = (Command s t (fst $ f d mus), snd $ f d mus)
    f d (Parallel muss) = (Parallel (fst results), snd results) where
        results = foldr (\mus (accum,_) -> ((fst $ f d mus):accum,(snd $ f d mus))) ([],d) muss
    f d (Grace mus1 mus2) = (Grace (fst $ f (CommonDur "8" 0) mus1) (fst $ f d mus2), snd $ f d mus2)
    f d (Sequential muss) = (Sequential (fst results), snd results) where
        results = foldr (\mus (accum,lastdur) -> ((fst $ f lastdur mus):accum,(snd $ f lastdur mus))) ([],d) muss
    f d (Leaf p NoDur nis) = (Leaf p d nis, d)
    f _ (Leaf p dur nis) = (Leaf p dur nis, dur)

makeDurationRational :: Dur1 -> Duration
makeDurationRational (RationalDur r) = r
makeDurationRational (CommonDur base dots) = addDots (lookupDur base) dots
makeDurationRational NoDur = error "can't happen: note without dur after applying durs"

splitChords :: Tree2 -> Tree3
splitChords = f where
    f (Function s mus)      = Function s (f mus)
    f (Command s t mus)     = Command s t (f mus)
    f (Parallel muss)       = Parallel (map f muss)
    f (Grace mus1 mus2)     = Grace (f mus1) (f mus2)
    f (Sequential muss)     = Sequential (map f muss)
    f (Leaf (NoteName1 pc oct) d noteitems)  = (Leaf (NoteName3 pc oct) d noteitems)
    f (Leaf (Frequency1 hz) d noteitems)     = (Leaf (Frequency3 hz) d noteitems)
    f (Leaf (Chord1 ps) d noteitems) = Parallel [f $ Leaf p d noteitems | p <- ps]

putCodeOnNotes :: Tree3 -> Tree4
putCodeOnNotes = f where
    f (Function s mus)      = addAnnotationToEveryNote (LongCommand (s++" { ") " } ") (f mus)
    f (Command s t mus)     = addAnnotationToEveryNote (LongCommand s t) (f mus)
    f (Parallel muss)       = ParallelY (map f muss)
    f (Grace mus1 mus2)     = GraceY (f mus1) (f mus2)
    f (Sequential muss)     = SequentialY (map f muss)
    f (Leaf p d ni)         = LeafY p d (map noteItem1to2 ni)


addAnnotationToEveryNote :: NoteItem2 -> Tree4 -> Tree4
addAnnotationToEveryNote = f where
    f ann (ParallelY muss)       = ParallelY (map (f ann) muss)
    f ann (SequentialY muss)     = SequentialY (map (f ann) muss)
    f ann (LeafY p d noteitems)  = LeafY p d (ann:noteitems)
    f ann (GraceY mus1 mus2)     = GraceY (f ann mus1) (f ann mus2)

noteItem1to2 :: NoteItem1 -> NoteItem2
noteItem1to2 = f where
    f Tie = Tie2
    f (Articulation c) = Articulation2 c
    f (NoteCommand s) = NoteCommand2 s
    f (With s)        = NoteCommand2 s
    f (Cents d) = Cents2 d


lookupNoteName :: [(String,(PitchClass,Accidental))] -> Pitch3 -> Pitch5
lookupNoteName table (NoteName3 base oct) = case (lookup base table) of
        Nothing -> Error5 $ "unknown note name \""++base++"\"."
        Just (pc, acc) -> RegularNote pc (getOct oct) 0 (Just acc) where
            getOct "" = 2
            getOct s  = 2 + (length s) * (case (head s) of '\'' -> 1; ',' -> -1)
lookupNoteName _ (Frequency3 freq) = let
    halfSteps = logBase (2 ** (1/12)) (freq / 440)
    fractional = halfSteps - fromIntegral (truncate halfSteps) -- might be negative
    (oct, pc) = ((truncate halfSteps) + 9) `divMod` 12
    oct' = oct + 4 -- middle c is c4, has oct of 0
    pc' = toEnum pc :: PitchClass
    cents = fractional * 100 -- cents was a portion of a halfstep, this is # of cents
    ((pc'', oct''), cents'') = 
        if cents > 50
        then (rewriteUpHalfstep (pc', oct'), cents - 100)
        else if cents < -50
        then (rewriteDownHalfstep (pc', oct'), cents + 100)
        else ((pc', oct'), cents)
    in Frequency5 pc'' oct'' cents''

rewriteUpHalfstep :: (PitchClass,Octave) -> (PitchClass,Octave)
rewriteUpHalfstep (pc, oct) = 
    if pc == B
    then (C, oct+1)
    else (toEnum $ fromEnum pc + 1, oct)

rewriteDownHalfstep :: (PitchClass,Octave) -> (PitchClass,Octave)
rewriteDownHalfstep (pc, oct) = 
    if pc == C
    then (B, oct-1)
    else (toEnum $ fromEnum pc - 1, oct)

addCents :: (Pitch5,[NoteItem2]) -> (Pitch5,[NoteItem2])
addCents (p, []) = (p, [])
addCents (RegularNote pc oct cents maybeAcc, (Cents2 d):nis) =
    addCents (RegularNote pc oct (cents+d) maybeAcc, nis)
addCents (p, nis) = (p, nis)

fixCents :: Pitch5 -> Pitch5
fixCents (RegularNote pc oct cents maybeAcc) = 
    if cents > 50 || cents < -50
    then lookupNoteName [] (Frequency3 (((2 ** (1/12)) ** ((fromIntegral $ fromEnum pc) + ((fromIntegral oct - 4) * 12))) * 440))
    else RegularNote pc oct cents maybeAcc
fixCents p = p

refinePitches :: Tree4 -> Tree5
refinePitches (ParallelY ts) = ParallelY (map refinePitches ts)
refinePitches (SequentialY ts) = SequentialY (map refinePitches ts)
refinePitches (GraceY t1 t2) = GraceY (refinePitches t1) (refinePitches t2)
refinePitches (LeafY p3 d nis) = let
    p5 = lookupNoteName en p3
    (p5', nis') = addCents (p5, nis)
    p5'' = fixCents p5
    in LeafY p5'' d nis'

pitch5toPitch :: Tree5 -> Tree6
pitch5toPitch (ParallelY muss) = ParallelY (map pitch5toPitch muss)
pitch5toPitch (SequentialY muss) = SequentialY (map pitch5toPitch muss)
pitch5toPitch (GraceY mus1 mus2) = GraceY (pitch5toPitch mus1) (pitch5toPitch mus2)
pitch5toPitch (LeafY p d nis) = LeafY (f p) d nis where
    f (RegularNote pc oct cents maybeAcc) = (MakePitch { _pc = pc, _oct = oct, _cents = cents })
    f (Frequency5 pc oct cents) = MakePitch { _pc = pc, _oct = oct, _cents = cents }
    f (Error5 s) = error "oops, errors aren't implemented yet"

putInTime :: Tree6 -> [InTime (Pitch,[NoteItem2])]
putInTime (GraceY mus1 mus2) = putInTime mus2 -- !!!!
putInTime (LeafY p d nis) = [InTime { _val = (p,nis), _dur = d, _t = 0 }]
putInTime (ParallelY muss) = sortBy (\n1 n2 -> (n1^.t) `compare` (n2^.t)) $ concat $ map putInTime muss
putInTime (SequentialY muss) = fst $ foldl (\(accum, time) mus -> (accum++(shiftLate time $ putInTime mus),time + (durMu mus))) ([],0) muss where
    shiftLate time itmus = map (\it -> it & t %~ (+time)) itmus
    durMu :: Tree6 -> Duration
    durMu (GraceY _ mus2) = durMu mus2
    durMu (ParallelY muss) = maximum (map durMu muss)
    durMu (SequentialY muss) = sum (map durMu muss)
    durMu (LeafY _ d _) = d 

allTransformations :: [(String,(PitchClass,Accidental))] -> Tree1 -> [InTime (Pitch,[NoteItem2])]
allTransformations table tree = tree 
    & fillInMissingDurs
    & makeAllDurationsRational
    & splitChords
    & putCodeOnNotes
    & refinePitches
    & pitch5toPitch
    & putInTime

test :: QuasiQuoter
test = QuasiQuoter { quoteExp = \s -> [| runParser musicParser () "" s >>= (Right . allTransformations en) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }
