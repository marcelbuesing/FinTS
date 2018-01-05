{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (pack)
import           Data.Currency as Currency
import qualified Data.ISO3166_CountryCodes as CC
import           Data.Time.Calendar (fromGregorian)
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
  , testCase ":60F:FirstOpeningBalance" firstOpeningBalanceTest
  , testCase ":60M:IntermediateOpeningBalance" intermediateOpeningBalanceTest
  , testCase ":61:StatementLine" statementLineTest
  , testCase ":86:InformationToAccountOwner" informationToAccountOwnerTest
  , testCase ":86:InformationToAccountOwnerMultiLine" informationToAccountOwnerMultiLineTest
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

firstOpeningBalanceTest :: Assertion
firstOpeningBalanceTest =
  parseOnly firstOpeningBalance raw' @?= Right openingBalance'
  where
    raw' = ":60F:C120302EUR16234,13"
    date' = fromGregorian 2012 03 02
    curr' = Currency.EUR
    amount' = Amount 16234.13
    openingBalance' = FirstOpeningBalance Credit date' curr' amount'

intermediateOpeningBalanceTest :: Assertion
intermediateOpeningBalanceTest =
  parseOnly intermediateOpeningBalance raw' @?= Right iOpeningBalance'
  where
    raw' = ":60M:C111111EUR960,"
    date' = fromGregorian 2011 11 11
    curr' = Currency.EUR
    amount' = Amount 960.0
    iOpeningBalance' = IntermediateOpeningBalance Credit date' curr' amount'


statementLineTest :: Assertion
statementLineTest =
  parseOnly statementLine raw' @?= Right statementLine'
  where
    raw' = ":61:1202290229C16,31NTRFNONREF//25-752443-1"
    valueDate' = fromGregorian 2012 02 29
    entryDate' = Just $ fromGregorian 2012 02 29
    amount' = Amount 16.31
    custRef' = CustomerReference "TRFNONREF"
    bankRef' = Just $ BankReference "25-752443-1"
    statementLine' = StatementLine valueDate' entryDate' Credit Nothing amount' N custRef' bankRef' Nothing

informationToAccountOwnerTest :: Assertion
informationToAccountOwnerTest =
  parseOnly informationToAccountOwner raw @?= Right info'
  where
    raw = ":86:999PN5477SCHECK-NR. 0000016703074"
    info' = InformationToAccountOwner ["999PN5477SCHECK-NR. 0000016703074"]

informationToAccountOwnerMultiLineTest :: Assertion
informationToAccountOwnerMultiLineTest =
  parseOnly informationToAccountOwner raw @?= Right info'
  where
    raw = ":86:999PN5477SCHECK-NR. 0000016703074\n999PN0920WECHSEL"
    info' = InformationToAccountOwner ["999PN5477SCHECK-NR. 0000016703074", "999PN0920WECHSEL"]
