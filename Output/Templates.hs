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

basicStaff :: [Instrument] -> String -> String
basicStaff insts contents = [r|\new Staff \with {
midiInstrument = "|] ++ (insts !! 0)^.midiInstrument ++ [r|"
instrumentName = "|] ++ (concat $ intersperse " & " $ map (^.name) insts) ++ [r|"
shortInstrumentName = "|] ++ (concat $ intersperse " & " $ map (^.shortName) insts) ++ [r|"
} { 
\once \override Staff.TimeSignature #'stencil = ##f 
\clef bass
|] ++ contents ++ [r|
\bar "|."}
|]

-- operating on notes:
errorNoAccidental :: String
errorNoAccidental = [r|^\\markup{\\sharp\\flat}|]

errorRedNotehead :: String
errorRedNotehead = [r|  \tweak #'font-size #8 \tweak #'color #red |]

-- etc
markupText :: String -> String
markupText s = "^\\markup{"++s++"}"