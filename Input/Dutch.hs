{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Input.Dutch where

import Input.Parse
import Language.Haskell.TH.Quote
import Language.Haskell.TH 
import Text.ParserCombinators.Parsec
import Tables

music :: QuasiQuoter
music = QuasiQuoter { quoteExp = \s -> [| handleParseError $ runParser musicParser () "" s >>= (Right . allTransformations nl) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }

p :: QuasiQuoter
p = QuasiQuoter { quoteExp = \s -> [| extractFirstPitch$ handleParseError $ runParser musicParser () "" s >>= (Right . allTransformations nl) |], quotePat = undefined, quoteType = undefined, quoteDec = undefined }
