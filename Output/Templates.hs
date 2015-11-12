module Output.Templates where

-- operating on music:
basicBeginScore = "\\version \"2.16.2\"\n\\language \"english\"\n\\score { <<\n"
basicEndScore = ">> \n\\layout { }\n\\midi { }\n}"

basicBeginStaff = "\\new Staff \\with {\nmidiInstrument = \"bassoon\"\n} { \n\\once \\override Staff.TimeSignature #'stencil = ##f \n\\clef bass\n\\cadenzaOn "
basicEndStaff = "\n\\cadenzaOff\n \\bar \"|\"\n}\n"

-- operating on notes:
errorNoAccidental :: String
errorNoAccidental = "^\\markup{\\sharp\\flat}"

-- etc
markupText :: String -> String
markupText s = "^\\markup{"++s++"}"