{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.RenderTypes where

import Types

type ChordR = [Note Ly]
data LinearNote = ChordR ChordR | UniNote (InTime (Note Ly)) | Function ExprCommand LinearNote
    deriving (Show)
type Linear = [LinearNote]
type Polyphony = [Linear]
type Staff = [Polyphony]
type Stage1 = [Staff]