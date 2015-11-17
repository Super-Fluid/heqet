{-# LANGUAGE QuasiQuotes #-}
module Output.Templates where

import Text.RawString.QQ

-- operating on music:
basicBeginScore = [r|
\version "2.16.2"
\language "english"
\score { <<
|]
basicEndScore = [r|
>> 
\layout { }
\midi { }
}|]

basicBeginStaff = [r|\new Staff \with {
midiInstrument = "bassoon"
instrumentName = "foo"
shortInstrumentName = "f"
} { 
\once \override Staff.TimeSignature #'stencil = ##f 
\clef bass
\cadenzaOn |]
basicEndStaff = [r|
\cadenzaOff
\bar "|."}
|]

-- operating on notes:
errorNoAccidental :: String
errorNoAccidental = [r|^\\markup{\\sharp\\flat}|]

-- etc
markupText :: String -> String
markupText s = "^\\markup{"++s++"}"