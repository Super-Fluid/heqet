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

data Pitch1 = NoteName String String
    | Frequency1 Double
    | Chord1 [Pitch1]
    deriving (Show)

data Pitch3 = NoteName3 String String
    | Frequency3 Double
    deriving (Show)

data NoteItem = Tie 
    | Articulation Char
    | NoteCommand String
    | With String
    | Cents Double
    deriving (Show)

data TreeX p d = Function String [TreeX p d]
    | Command String String [TreeX p d]
    | Leaf p d [NoteItem]
    | Voices [[TreeX p d]]
    | Grace [TreeX p d] [TreeX p d]
    deriving (Show)

type Tree1 = TreeX Pitch1 Dur1
type Tree2 = TreeX Pitch1 Duration
type Tree3 = TreeX Pitch3 Duration


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
musicParser = many tree <* eof

tree :: Parser Tree1
tree = do
    whiteSpace
    r <- root
    whiteSpace
    return r

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
    music <- many (try tree)
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
    music <- many tree
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
    return $ NoteName name oct

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

noteItem :: Parser NoteItem
noteItem = (tie <|> try articulation <|> try with <|> try cents1 <|> try noteCommand) <* whiteSpace

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

cents1 :: Parser NoteItem
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

noteCommand :: Parser NoteItem
noteCommand =  do
    char '\\'
    name <- many1 alphaNum
    return $ NoteCommand name

voices1 :: Parser Tree1
voices1 = do 
    string "<<"
    whiteSpace
    vs <- (braces (many $ try tree)) `sepBy` voicesSep
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
makeAllDurationsRational (Function s mus) = Function s (map makeAllDurationsRational mus)
makeAllDurationsRational (Command s t mus) = Command s t (map makeAllDurationsRational mus)
makeAllDurationsRational (Leaf p d noteitems) = Leaf p (makeDurationRational d) noteitems
makeAllDurationsRational (Voices muss) = Voices (map (map makeAllDurationsRational) muss)
makeAllDurationsRational (Grace mus1 mus2) = Grace (map makeAllDurationsRational mus1) (map makeAllDurationsRational mus2)

makeDurationRational :: Dur1 -> Duration
makeDurationRational (RationalDur r) = r
makeDurationRational (CommonDur base dots) = addDots (lookupDur base) dots

-- splitChords :: Tree2 -> Tree3
