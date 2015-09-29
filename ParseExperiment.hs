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
import Types

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

data TreeY p d = LeafY p d [NoteItem2]
    | ParallelY [TreeY p d]
    | SequentialY [TreeY p d]
    | GraceY (TreeY p d) (TreeY p d)
    deriving (Show)

type Tree1 = TreeX Pitch1 Dur1
type Tree2 = TreeX Pitch1 Duration
type Tree3 = TreeX Pitch3 Duration
type Tree4 = TreeY Pitch3 Duration


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


demo :: QuasiQuoter
demo = QuasiQuoter { quoteExp = \s -> [|  runParser musicParser () "" s |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }

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

makeDurationRational :: Dur1 -> Duration
makeDurationRational (RationalDur r) = r
makeDurationRational (CommonDur base dots) = addDots (lookupDur base) dots

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