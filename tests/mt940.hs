{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import           Data.ISO3166_CountryCodes (CountryCode(..))
import           Test.Tasty
import           Test.Tasty.HUnit

import FinTS.Data.ISO9362BIC
import FinTS.Data.IBAN
import FinTS.Data.MT940

main :: IO ()
main = defaultMain $ testGroup "NMEA"
  [ testCase "BIC" bicTest
  , testCase ":20:TransactionReferenceNumber" transactionReferenceNumberTest
  , testCase ":25:AccountIdentification" accountIdentificationTest
  , testCase ":28C:StatementNumberSeqNumber" statementNumberSeqNumberTest
  ]

bicTest :: Assertion
bicTest =
  parseOnly bic raw' @?= Right bic'
  where
    raw' = "BELADEBEXXX"
    bic' = BIC (BankCode "BELA") DE (LocationCode "BE") MainOffice

transactionReferenceNumberTest :: Assertion
transactionReferenceNumberTest =
  parseOnly transactionReferenceNumber raw @?= Right expected
  where raw = ":20:s000000000587017"
        expected = TransactionReferenceNumber "s000000000587017"

accountIdentificationTest :: Assertion
accountIdentificationTest =
  parseOnly accountIdentification ":25:NL08DEUT0319809633EUR" @?= Right id'
    where cd' = ISO7064CheckDigit "08"
          iban' = IBAN NL cd' [IBANCharGrouping "DEUT", IBANDigitGrouping "0319809633"]
          id' = AccountIdentification iban'

statementNumberSeqNumberTest :: Assertion
statementNumberSeqNumberTest =
  parseOnly statementNumberSeqNumber ":28C:5/1" @?= Right seqNum'
  where seqNum' = StatementNumberSeqNumber (StatementNumber 5) (Just $ SeqNumber 1)
