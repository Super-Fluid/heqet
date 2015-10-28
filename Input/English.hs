{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Input.English where

import Input.Parse
import Language.Haskell.TH.Quote
import Language.Haskell.TH 
import Text.ParserCombinators.Parsec
import Tables

music :: QuasiQuoter
music = QuasiQuoter { quoteExp = \s -> [| handleParseError $ runParser musicParser () "" s >>= (Right . allTransformations en) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }
