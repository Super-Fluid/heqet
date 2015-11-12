module Example where

import Heqet
import Input.English

import Control.Lens

main = quickScore [music| c2 d4 e f g a b c'2\fermata |]