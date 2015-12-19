{-# LANGUAGE QuasiQuotes #-}
module Heqet.Example where

import Heqet
import Heqet.Input.English

import Control.Lens

main = quickLine [music| c2 d4 e f g a b c'2\fermata |]