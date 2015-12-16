{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Heqet.Split where

import Heqet.Types
import Heqet.Tools
import Heqet.Input.English

import Control.Lens

superBasicSplit :: Music -> Music
superBasicSplit = concatMap (\n ->
    if isPlayable (n^.val.pitch)
    then 
        if getNoteHeight n == Nothing || getNoteHeight n > getPitchHeight [pp| c' |]
        then [n & val.subStaff .~ Just RH]
        else [n & val.subStaff .~ Just LH]
    else [n & val.subStaff .~ Just RH, n & val.subStaff .~ Just LH] -- put on both events
    )