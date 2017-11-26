{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import           Test.Tasty
import           Test.Tasty.HUnit

import FinTS.Data.MT940


transactionReferenceNumberTest :: Assertion
transactionReferenceNumberTest =
  parseOnly transactionReference raw @?= Right eGphdt
  where raw = ":20:s000000000587017"
        expected = TransactionReference "s000000000587017"

