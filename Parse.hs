{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Parse where

import Types
import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe)
import qualified Data.Either
import Language.Haskell.TH.Quote
import Language.Haskell.TH 

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
   ,("0",0)
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

inputPitch :: GenParser Char st Pitch'
inputPitch = do
    spaces
    pc <- pitchClass
    o <- octave
    return $ RegPitch $ Pitch { _pc = pc, _oct = o, _cents = 0 }

rest :: GenParser Char st Pitch'
rest = do
    spaces
    char 'r'
    return Rest

inputPitchEtc :: GenParser Char st Pitch'
inputPitchEtc = (try inputPitch) <|> rest

inputNote :: GenParser Char st (Note,Duration)
inputNote = do
    spaces
    p <- inputPitchEtc
    d <- duration
    spaces
    return (Note { _pitch = p, _acc = Natural, _noteCommands = [], _exprCommands = [] }, d)

notes2music :: [(Note,Duration)] -> Music' Note
notes2music xs = foldl addToMusic ([],0) xs & (^._1)
    where addToMusic (m, time) (note, d) = (m ++ [InTime {_val = note, _dur = d, _t = time}], time+d)

parseMusic :: GenParser Char st (Music' Note)
parseMusic = do
    notes <- many inputNote 
    return $ notes2music notes

music :: QuasiQuoter
music = QuasiQuoter { quoteExp = \s -> [| runParser parseMusic () "" s |] }