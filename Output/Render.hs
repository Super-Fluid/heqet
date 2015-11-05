{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.Render where

import Types
import qualified Tables
import Output.Templates
import Tools
import Output.RenderTypes
import List
import qualified Output.LilypondSettings

import Control.Lens
import Data.Maybe (fromJust,isJust,catMaybes)
import Data.Tuple (swap)
import Data.List (concat, intersperse, sortBy, sort, group, find)
import Control.Applicative ((<$>),(<|>))
import Data.Monoid ((<>))
import Safe (readMay)

testRender :: Music -> String
testRender mu = "\\version \"2.16.2\"\n\\language \"english\"\n\\score {\n\\new Staff \\with {\nmidiInstrument = \"bassoon\"\n} { \n\\once \\override Staff.TimeSignature #'stencil = ##f \n\\clef bass\n\\cadenzaOn " ++ (listRender mu) ++ "\n\\cadenzaOff\n \\bar \"|\"\n}\\layout { }\n\\midi { }\n}"

listRender :: Music -> String
listRender intimes = concat $ intersperse " " $ map toLy $ map (\i -> (i^.val,i^.dur)) intimes

toLy (note,dur) = renderPitch (note^.pitch) (note^.acc) ++ renderDuration (dur)

renderPitch :: Ly -> (Maybe Accidental) -> String
renderPitch (Pitch p) acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)
renderPitch Rest _ = "r"
renderPitch (Perc p) _ = "c"

renderPitchAcc :: PitchClass -> (Maybe Accidental) -> String
renderPitchAcc pc (Just acc) = fromJust $ lookup (pc,acc) (map swap Tables.en)
renderPitchAcc pc Nothing = fromJust $ lookup pc (map (\((p,a),s) -> (p,s)) (map swap Tables.en))

commonDurations :: [(Duration,String)]
commonDurations = [
     (1,"4")
    ,(2,"2")
    ,(4,"1")
    ,(8,"\\breve")
    ,(1/2,"8")
    ,(1/4,"16")
    ,(1/8,"32")
    ,(1/16,"64")
    ,(1/32,"128")
    ,(1/64,"256")
    ,(3/2,"4.")
    ,(3,"2.")
    ,(6,"1.")
    ,(12,"\\breve.")
    ,(3/4,"8.")
    ,(3/8,"16.")
    ,(3/16,"32.")
    ,(3/32,"64.")
    ,(3/64,"128.")
    ,(3/128,"256.")
     ]

renderDuration :: Duration -> String
renderDuration dur
    | isJust (lookup dur commonDurations) = fromJust (lookup dur commonDurations)
    | otherwise = "1"

renderOct :: Octave -> String
renderOct oct
    | oct == 0 = ""
    | oct < 0 = replicate (- oct) ','
    | otherwise = replicate (oct) '\''

data WrittenNote = WrittenNote { 
      _preceeding :: [String]
    , _body :: String
    , _duration :: Duration
    , _noteItems :: [String]
    , _following :: [String]
    }
    deriving (Show)
makeLenses ''WrittenNote

type NoteInProgress = (Note Ly, WrittenNote)

startRenderingNote :: InTime (Note Ly) -> NoteInProgress
startRenderingNote it = (it^.val, WrittenNote [] "" (it^.dur) [] [])

renderNoteBodyInStaff :: NoteInProgress -> NoteInProgress
renderNoteBodyInStaff (n, w) = let
    body' = case n^.pitch of 
        Pitch p -> renderPitch' p (n^.acc)
        Perc s -> xNote n
        Rest -> "r"
        Effect -> xNote n
        Lyric s -> xNote n
        Grace mus -> "\\grace {" ++ renderInStaff mus ++ "}"
    nis' = case n^.pitch of
        Pitch _ -> w^.noteItems 
        Perc s -> (markupText s):(w^.noteItems)
        Rest -> w^.noteItems
        Effect -> w^.noteItems
        Lyric s -> (markupText s):(w^.noteItems)
        Grace mus -> w^.noteItems
    in (n, w & noteItems .~ nis' & body .~ body')

renderNoteItems :: NoteInProgress -> NoteInProgress
renderNoteItems (n,w) = let
    beginExpr = map (^.begin) (n^.exprCommands)
    endExpr = map (^.end) (n^.exprCommands)
    beginNDExpr = map (^.begin) (n^.nonDistCommands)
    endNDExpr = map (^.end) (n^.nonDistCommands)
    markedErrors = map markupText $ n^.errors
    articulations = map renderArt $ n^.artics
    w' = w
        & preceeding .~ beginExpr ++ beginNDExpr
        & following .~ endExpr ++ endNDExpr
        & noteItems .~ (map ('\\':) $ n^.noteCommands) ++ articulations ++ markedErrors
    in (n,w')

renderArt :: SimpleArticulation -> String
renderArt Staccato = "-."
renderArt Marcato = "-^"
renderArt Tenuto = "--"
renderArt Portato = "-_"
renderArt Staccatissimo = "-!"
renderArt Stopped = "-+"
renderArt Accent = "->"

renderInStaff :: Music -> String
renderInStaff mus = concat $ intersperse " " $ map f mus where
    f note = note
        & startRenderingNote
        & renderNoteBodyInStaff
        & renderNoteItems
        & extractRenderedNote

extractRenderedNote :: NoteInProgress -> String
extractRenderedNote (n,w) = 
    (concat $ w^.preceeding)
    ++ (w^.body)
    ++ (renderDuration $ w^.duration)
    ++ (concat $ w^.noteItems)
    ++ (concat $ reverse $ w^.following)

xNote :: Note Ly -> String
xNote n = let 
    fakePitch = case n^.clef of
        Just Treble  -> "b'"
        Just Alto    -> "c'"
        Just Treble8 -> "b"
        Just Tenor   -> "a"
        Just Bass    -> "d"
        _            -> "c'"
    in "\\xNote "++fakePitch

renderPitch' :: Pitch -> (Maybe Accidental) -> String
renderPitch' p acc = renderPitchAcc (p^.pc) acc ++ renderOct (p^.oct)

toStage0 :: Music -> [Linear]
toStage0 mus = map phraseToLinear musicInLines where
    musicInLines = (allVoices mus) & map (\v -> mus^.voice v)

allVoices :: Music -> [String]
allVoices = catMaybes.(map head).group.sort.(map (\it -> lookup "voice" (it^.val.tags)))

phraseToLinear :: Music -> Linear
phraseToLinear = map (\it -> it & val .~ (UniNote $ it^.val))

isChordable :: InTime a -> InTime a -> Bool
isChordable it1 it2 = (it1^.dur == it2^.dur) && (it1^.t == it2^.t)

formChord :: [InTime LinearNote] -> InTime LinearNote
formChord [it] = it
formChord its = (head its) & val .~ ChordR (its & map (^.val) & map (\(UniNote n) -> n))

combineChords :: Linear -> Linear
combineChords mus = mus 
    & makeBucketsBy isChordable
    & sortBy (\a b -> ((head a)^.t) `compare` ((head b)^.t))
    & map formChord

pitchLN :: LinearNote -> Double
pitchLN (UniNote n) = pitch2num n
pitchLN (ChordR ns) = (sum $ map pitch2num ns) / (fromIntegral $ length ns)

pitch2num :: Note Ly -> Double
pitch2num n = let x = ly2num (n^.pitch)
    in  if isJust x
        then fromJust x
        else fromJust $ (lookup "verse" (n^.tags) >>= readMay >>= return.(0-)) <|> Just 0

ly2num :: Ly -> Maybe Double
ly2num (Pitch p) = Just (((2 ** (1/12)) ** ((fromIntegral $ ((fromEnum (p^.pc) + 3) `mod` 12)) + ((fromIntegral (p^.oct) - 4) * 12) + ((p^.cents)/100))) * 440)
ly2num Rest = Just 0
ly2num (Perc _) = Just 0 -- expand on this according to common drum notation?
ly2num (Lyric _) = Nothing
ly2num (Grace _) = Just 0 

timePitchSort :: Linear -> Linear
timePitchSort = sortBy $ \it1 it2 -> (it1^.t) `compare` (it2^.t) <> (pitchLN $ it1^.val) `compare` (pitchLN $ it2^.val)

findPolys :: Linear -> Staff
findPolys lin = reverse $ foldl f [] (timePitchSort lin) where
    f [] it = [ emptyCol & atIndex 1 .~ [it] ]
    f (current:past) it = let (voiceN,succeeded) = tryToFit it current
        in if not succeeded
           then (emptyCol & atIndex 1 .~ [it]):current:past
           else if voiceN == 1
                then if all (checkLineFit it) current
                     then (emptyCol & atIndex 1 .~ [it]):current:past
                     else (current & atIndex 1 %~ (it:)):past
                else (current & atIndex voiceN %~ (it:)):past
    emptyCol = replicate Output.LilypondSettings.maxNumberOfVoices []
    tryToFit :: (InTime LinearNote) -> Polyphony -> (Int,Bool)
    tryToFit it col = tryToFitHelper $ checkFit it col
    tryToFitHelper Nothing = (undefined,False)
    tryToFitHelper (Just i) = (i,True)
    checkFit it col = snd <$> find (checkLineFit it . fst) (zipWith (,) col [0..])
    checkLineFit it line = all (not . conflictsWith it) line
    conflictsWith it1 it2 = it1^.t < (it2^.t + it2^.dur) && it2^.t > (it1^.t + it1^.dur)


allRendering :: Music -> Stage1
allRendering mus = mus
    & toStage0
    & map combineChords
    & map timePitchSort
    & map findPolys
    & (map.map.map) reverse -- put each Linear in order
    & map reverse -- put the Polyphonys in time order
    & (map.map) (filter (not.null)) -- remove empty Linears from each Polyphony