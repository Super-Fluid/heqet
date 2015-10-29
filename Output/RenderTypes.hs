{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.RenderTypes where

import Types

type ChordR = [Note Ly]
data LinearNote = ChordR ChordR | UniNote (Note Ly)
type Linear = [LinearNote]
type Polyphony = [Linear]
type Staff = [Polyphony]
type Stage1 = [Staff]