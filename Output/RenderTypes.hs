{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Output.RenderTypes where

import Types

type ChordR = [Note Ly]
data LinearNote = ChordR ChordR | UniNote (Note Ly) | Function ExprCommand Linear
    deriving (Show)
type Linear = [InTime LinearNote]
type Polyphony = [Linear]
type Staff = [Polyphony]
type Stage1 = [Staff]