{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Heqet.Input.English where

import Heqet.Input.Parse
import Heqet.Tables

import Language.Haskell.TH.Quote
import Language.Haskell.TH 
import Text.ParserCombinators.Parsec

music :: QuasiQuoter
music = QuasiQuoter { quoteExp = \s -> [| handleParseError $ runParser musicParser () "" s >>= (Right . allTransformations en) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }

pp :: QuasiQuoter
pp = QuasiQuoter { quoteExp = \s -> [| extractFirstPitch$ handleParseError $ runParser musicParser () "" s >>= (Right . allTransformations en) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }
