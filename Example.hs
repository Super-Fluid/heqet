{-# LANGUAGE QuasiQuotes #-}
module Example where

import Heqet
import Input.English

import Control.Lens

main = quickLine [music| c2 d4 e f g a b c'2\fermata |]