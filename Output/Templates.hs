module Output.Templates where

-- operating on music:


-- operating on notes:
errorNoAccidental :: String
errorNoAccidental = "^\\markup{\\sharp\\flat}"

-- etc
markupText :: String -> String
markupText s = "^\\markup{"++s++"}"