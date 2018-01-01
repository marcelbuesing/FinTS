{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import           Data.ISO3166_CountryCodes (CountryCode(..))
import           Test.Tasty
import           Test.Tasty.HUnit

import FinTS.Data.ISO9362BIC
import FinTS.Data.ISO7064CheckDigits
import FinTS.Data.ISO13616IBAN
import FinTS.Data.MT940

main :: IO ()
main = defaultMain $ testGroup "NMEA"
  [ testCase "BIC main office" bicMainOfficeTest
  , testCase "BIC 8 char" bicEightCharTest
  , testCase "BIC with branch" bicBranchTest
  , testCase ":20:TransactionReferenceNumber" transactionReferenceNumberTest
  , testCase ":25:AccountIdentification" accountIdentificationTest
  , testCase ":28C:StatementNumberSeqNumber" statementNumberSeqNumberTest
  ]

bicMainOfficeTest :: Assertion
bicMainOfficeTest =
  parseOnly bic raw' @?= Right bic'
  where
    raw' = "BELADEBEXXX"
    bic' = BIC (BankCode "BELA") DE (LocationCode "BE") MainOffice

bicEightCharTest :: Assertion
bicEightCharTest =
  parseOnly bic raw' @?= Right bic'
  where
    raw' = "MARKDEFF"
    bic' = BIC (BankCode "MARK") DE (LocationCode "FF") MainOffice

bicBranchTest :: Assertion
bicBranchTest =
  parseOnly bic raw' @?= Right bic'
  where
    raw' = "UBSWCHZH80A"
    bic' = BIC (BankCode "UBSW") CH (LocationCode "ZH") (Branch "80A")

transactionReferenceNumberTest :: Assertion
transactionReferenceNumberTest =
  parseOnly transactionReferenceNumber raw @?= Right expected
  where raw = ":20:s000000000587017"
        expected = TransactionReferenceNumber "s000000000587017"

accountIdentificationTest :: Assertion
accountIdentificationTest =
  parseOnly accountIdentification ":25:NL08DEUT0319809633EUR" @?= Right id'
    where cd' = CheckDigits "08"
          iban' = IBAN NL cd' [IBANCharGrouping "DEUT", IBANDigitGrouping "0319809633"]
          id' = AccountIdentification iban'

statementNumberSeqNumberTest :: Assertion
statementNumberSeqNumberTest =
  parseOnly statementNumberSeqNumber ":28C:5/1" @?= Right seqNum'
  where seqNum' = StatementNumberSeqNumber (StatementNumber 5) (Just $ SeqNumber 1)
