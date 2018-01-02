{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import qualified Data.ISO3166_CountryCodes as CC
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified FinTS.Data.ISO9362_BIC as BIC
import           FinTS.Data.ISO7064_CheckDigits
import qualified FinTS.Data.ISO13616_IBAN as IBAN
import           FinTS.Data.MT940

main :: IO ()
main = defaultMain $ testGroup "NMEA"
  [ testCase "BIC main office" bicMainOfficeTest
  , testCase "BIC 8 char" bicEightCharTest
  , testCase "BIC with branch" bicBranchTest
  , testCase "IBAN Germany" ibanGermanyTest
  , testCase ":20:TransactionReferenceNumber" transactionReferenceNumberTest
  , testCase ":25:AccountIdentification" accountIdentificationTest
  , testCase ":28C:StatementNumberSeqNumber" statementNumberSeqNumberTest
  ]

bicMainOfficeTest :: Assertion
bicMainOfficeTest =
  parseOnly BIC.bic raw' @?= Right bic'
  where
    raw' = "BELADEBEXXX"
    bic' = BIC.BIC (BIC.BankCode "BELA") CC.DE (BIC.LocationCode "BE") BIC.MainOffice

bicEightCharTest :: Assertion
bicEightCharTest =
  parseOnly BIC.bic raw' @?= Right bic'
  where
    raw' = "MARKDEFF"
    bic' = BIC.BIC (BIC.BankCode "MARK") CC.DE (BIC.LocationCode "FF") BIC.MainOffice

bicBranchTest :: Assertion
bicBranchTest =
  parseOnly BIC.bic raw' @?= Right bic'
  where
    raw' = "UBSWCHZH80A"
    bic' = BIC.BIC (BIC.BankCode "UBSW") CC.CH (BIC.LocationCode "ZH") (BIC.Branch "80A")

ibanGermanyTest :: Assertion
ibanGermanyTest =
  parseOnly IBAN.iban raw' @?= Right iban'
  where
    raw'  = "DE89370400440532013000"
    blz'  = IBAN.BLZ $ IBAN.NationalBankCode "37040044"
    acc'  = IBAN.AccountNumber "0532013000"
    iban' = IBAN.IBAN CC.DE (CheckDigits "89") (IBAN.BBAN_DE blz' acc')

transactionReferenceNumberTest :: Assertion
transactionReferenceNumberTest =
  parseOnly transactionReferenceNumber raw @?= Right expected
  where raw = ":20:s000000000587017"
        expected = TransactionReferenceNumber "s000000000587017"

accountIdentificationTest :: Assertion
accountIdentificationTest =
  parseOnly accountIdentification ":25:NL08DEUT0319809633EUR" @?= Right id'
    where cd' = CheckDigits "08"
          iban' = IBAN.IBAN CC.NL cd' (IBAN.BBAN_NL (BIC.BankCode "DEUT") (IBAN.AccountNumber "0319809633"))
          id' = AccountIdentification iban'

statementNumberSeqNumberTest :: Assertion
statementNumberSeqNumberTest =
  parseOnly statementNumberSeqNumber ":28C:5/1" @?= Right seqNum'
  where seqNum' = StatementNumberSeqNumber (StatementNumber 5) (Just $ SeqNumber 1)
