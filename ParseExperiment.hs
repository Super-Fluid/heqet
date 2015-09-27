module ParseExperiment where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

type PitchStr = String
type DurStr = String

data Dur1 = NoDur 
    | RationalDur Rational 
    | CommonDur String Int -- will need to be looked up later
    deriving (Show)

data Pitch1 = NoteName String String
    | Frequency Double
    | Chord [Pitch1]
    deriving (Show)

data NoteItem = Tie 
    | Articulation Char
    | NoteCommand String
    | With String
    | Cents Double
    deriving (Show)

data Tree1 = Function String [Tree1]
    | Command String String [Tree1]
    | Leaf Pitch1 Dur1 [NoteItem]
    | Voices [Tree1]
    | Grace [Tree1] [Tree1]
    deriving (Show)

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

musicParser :: Parser [Tree1]
musicParser = many tree

tree :: Parser Tree1
tree = do
    whiteSpace
    r <- root
    whiteSpace
    return r

root :: Parser Tree1
root = try function <|> try command <|> try leaf <|> voices <|> grace

function :: Parser Tree1
function = do
    string "\\function"
    whiteSpace
    func <- braces $ stringLiteral
    whiteSpace
    music <- many tree
    whiteSpace
    string "\\end"
    whiteSpace
    return $ Function func music

command :: Parser Tree1
command = do
    string "\\command"
    whiteSpace
    begin <- braces $ stringLiteral
    whiteSpace
    end <- braces $ stringLiteral
    whiteSpace
    music <- many tree
    whiteSpace
    string "\\end"
    whiteSpace
    return $ Command begin end music

leaf :: Parser Tree1
leaf = do
    p <- pitch
    dur <- duration
    whiteSpace
    items <- noteItem `sepBy` whiteSpace
    whiteSpace
    return $ Leaf p dur items

pitch :: Parser Pitch1
pitch = noteName <|> frequency <|> chord

noteName :: Parser Pitch1
noteName = do
    name <- many1 lower
    oct <- many $ oneOf "',"
    return $ NoteName name oct

frequency :: Parser Pitch1
frequency = do
    hz <- num
    string "hz"
    return $ Frequency hz

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
    ps <- pitch `sepBy` whiteSpace
    whiteSpace
    char '>'
    return $ Chord ps

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

noteItem :: Parser NoteItem
noteItem = tie <|> try articulation <|> try with <|> try cents <|> noteCommand

tie :: Parser NoteItem
tie = char '~' >> return Tie

articulation :: Parser NoteItem
articulation = do
    char '-'
    c <- oneOf "<>\\'+-!._,~/0123456789"
    return $ Articulation c


with :: Parser NoteItem
with = do
    string "\\with"
    whiteSpace
    cmd <- stringLiteral
    whiteSpace
    return $ With cmd

cents :: Parser NoteItem
cents = do
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

noteCommand :: Parser NoteItem
noteCommand =  do
    char '\\'
    name <- many1 alphaNum
    return $ NoteCommand name

voices :: Parser Tree1
voices = do 
    string "<<"
    whiteSpace
    vs <- (braces tree) `sepBy` voicesSep
    whiteSpace
    string ">>"
    return $ Voices vs

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
    g <- braces $ many tree
    whiteSpace
    n <- braces $ many tree
    whiteSpace
    return $ Grace g n