{-# LANGUAGE QuasiQuotes #-}
module Output.Templates where

import Types

import Text.RawString.QQ
import Control.Lens
import Data.List

-- operating on music:
basicScore :: String -> String
basicScore contents = [r|
\version "2.16.2"
\language "english"
\score { <<
|] ++ contents ++ [r|
>> 
\layout { 
    % let each staff have a separate time signature
  \context {
    \Score
    \remove "Timing_translator"
    \remove "Default_bar_line_engraver"
  }
  \context {
    \Staff
    \consists "Timing_translator"
    \consists "Default_bar_line_engraver"
  } 
}
\midi { }
}|]

renderInstrumentNames :: [Instrument] -> String
renderInstrumentNames insts = concat $ intersperse " & " $ map (^.name) insts

renderInstrumentShortNames :: [Instrument] -> String
renderInstrumentShortNames insts = concat $ intersperse " & " $ map (^.shortName) insts

renderMidiInstrument :: [Instrument] -> String
renderMidiInstrument [] = " "
renderMidiInstrument insts = head insts & view midiInstrument

basicStaff :: [Instrument] -> String -> String
basicStaff insts contents = [r|\new Staff \with {
midiInstrument = "|] ++ renderMidiInstrument insts ++ [r|"
instrumentName = "|] ++ renderInstrumentNames insts ++ [r|"
shortInstrumentName = "|] ++ renderInstrumentShortNames insts ++ [r|"
} { 
|] ++ contents ++ [r|
\bar "|."}
|]

unNamedStaff :: String -> String
unNamedStaff contents = [r|\new Staff { 
|] ++ contents ++ [r|
\bar "|."}
|]

pianoStaff :: [Instrument] -> [String] -> String
pianoStaff insts substaves = [r|\new PianoStaff <<
\set PianoStaff.midiInstrument = "|] ++ renderMidiInstrument insts ++ [r|"
\set PianoStaff.instrumentName = #"|] ++ renderInstrumentNames insts ++ [r|"
\set PianoStaff.shortInstrumentName = #"|] ++ renderInstrumentShortNames insts ++ [r|"
|] ++ (concat $ intersperse "\n" $ map unNamedStaff substaves) ++ [r|
>>
|]

-- operating on notes:
errorNoAccidental :: String
errorNoAccidental = [r|^\\markup{\\sharp\\flat}|]

errorRedNotehead :: String
errorRedNotehead = [r|  \tweak #'font-size #8 \tweak #'color #red |]

-- etc
markupText :: String -> String
markupText s = "^\\markup{"++s++"}"