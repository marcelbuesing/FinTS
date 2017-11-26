{-# LANGUAGE OverloadedStrings #-}

-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf
-- https://web.archive.org/web/20160725042101/http://www.societegenerale.rs/fileadmin/template/main/pdf/SGS%20MT940.pdf
-- https://www.db-bankline.deutsche-bank.com/download/MT940_Deutschland_Structure2002.pdf
-- http://www.sepaforcorporates.com/swift-for-corporates/account-statement-mt940-file-format-overview/
-- https://www.ibm.com/support/knowledgecenter/en/SSRH46_3.0.0/fxhmapr2016wtxswibtxtmt940.html

module FinTS.Data.MT940 where

import           Control.Applicative ((<|>))
import Data.ISO3166_CountryCodes (CountryCode)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM)

type MT940Date = Day

-- | https://www2.swift.com/uhbonline/books/public/en_uk/usgi_20160722/con_31519.htm
-- | Appendix Supported Characters - https://deutschebank.nl/nl/docs/MT94042_EN.pdf
isSwiftCharacter :: Char -> Bool
isSwiftCharacter c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') ||
    (c == '\040') || -- ` ` space
    (c == '\047') || -- `'`
    (c == '\050') || -- `(`
    (c == '\051') || -- `)`
    (c == '\053') || -- `+`
    (c == '\054') || -- `,`
    (c == '\055') || -- `-`
    (c == '\056') || -- `.`
    (c == '\057') || -- `/`
    (c == '\072') || -- `:`
    (c == '\077') || -- `?`
    (c == '\173') || -- `{`
    (c == '\175')    -- `}`

swiftCharacter :: Parser Char
swiftCharacter = satisfy isSwiftCharacter <?> "SwiftCharacter"

data CreditDebitMark = Credit | Debit deriving (Show)

creditDebit :: Parser CreditDebitMark
creditDebit = do
      (char 'C' >> return Credit)
  <|> (char 'D' >> return Debit)

data TransactionTypeIdentCode = N | F deriving (Show)

transactionTypeIdentCode :: Parser TransactionTypeIdentCode
transactionTypeIdentCode = do
      (char 'N' >> return N)
  <|> (char 'F' >> return F)

newtype BankReference = BankReference T.Text deriving (Show)
newtype TransactionNumber = TransactionNumber T.Text deriving (Show)
newtype IsoCurrencyCode = IsoCurrencyCode CountryCode deriving (Show)
newtype Amount = Amount Double deriving (Show, Eq, Ord)
newtype StatementNumber = StatementNumber Integer deriving (Show, Eq, Read)
newtype SeqNumber = SeqNumber Integer deriving (Show, Eq, Ord, Read)

-- | `:20:`
data TransactionReferenceNumber = TransactionReferenceNumber
    { _transactionReferenceNumber :: T.Text
    }  deriving (Show, Read)

transactionReferenceNumber :: Parser TransactionReferenceNumber
transactionReferenceNumber = do
    _ <- string ":20:" <?> "TransactionReferenceNumber Prefix"
    n <- T.pack <$> count 16 swiftCharacter <?> "TransactionReferenceNumber"
    return $ TransactionReferenceNumber n

-- | `:21:`
data RelatedReference = RelatedReference
    { _relatedReference :: T.Text
    } deriving (Show)

-- | `:25:`
data AccountIdentification = AccountIdentification {} deriving (Show)

-- | `:28C:`
data StatementNumberSeqNumber = StatementNumberSeqNumber
    { _statementNumber :: StatementNumber
    , _seqNumber       :: Maybe SeqNumber
    } deriving (Show)

statementNumberSeqNumber :: Parser StatementNumberSeqNumber
statementNumberSeqNumber = do
    _ <- string ":28C:" <?> "StatementNumberSeqNumber Prefix"
    stn <- StatementNumber . read <$> count 5 digit <?> "StatementNumber"
    sqn <- option Nothing $ char '/' *>  ((Just . SeqNumber . read) <$> count 5 digit <?> "SequenceNumber")
    return $ StatementNumberSeqNumber stn sqn

-- | `:60a:`
data OpeningBalance = OpeningBalance
    { _openingBalanceMark :: CreditDebitMark
    , _openingBalanceStatementDate :: MT940Date
    , _openingBalanceCurrency :: IsoCurrencyCode
    , _openingBalanceAmount :: Amount
    } deriving (Show)

-- | `:60F:`
data FirstOpeningBalance = FirstOpeningBalance
    { _firstOpeningBalanceMark :: CreditDebitMark
    , _firstOpeningBalanceStatementDate :: MT940Date
    , _firstOpeningBalanceCurrency :: IsoCurrencyCode
    , _firstOpeningBalanceAmount :: Amount
    } deriving (Show)

-- | `:60M:`
data IntermediateBalance = IntermediateBalance
    { _intermediateBalanceMark :: CreditDebitMark
    , _intermediateBalanceStatementDate :: MT940Date
    , _intermediateBalanceCurrency :: IsoCurrencyCode
    , _intermediateBalanceAmount :: Amount
    } deriving (Show)

-- | `:61:`
data StatementLine = StatementLine
    { _statementLineValueDate :: MT940Date
    , _statementLineMark :: CreditDebitMark
    , _statementLineAmount :: Amount
    , _statementLineTransactionTypeIdentificationCode :: TransactionTypeIdentCode
    , _statementLineTransactionNumber :: TransactionNumber
    , _statementLineBankReference :: BankReference
    , _statementLineSupplementaryDetails :: T.Text
    } deriving (Show)

-- | `:62:`
data ClosingBalance = ClosingBalance
    { _closingBalanceMark :: CreditDebitMark
    , _closingBalanceStatementDate :: MT940Date
    , _closingBalanceCurrency :: IsoCurrencyCode
    , _closingBalanceAmount :: Amount
    } deriving (Show)

-- | `:62M:`
data IntermediateClosingBalance = IntermediateClosingBalance
    { _intermediateClosingBalanceMark :: CreditDebitMark
    , _intermediateClosingBalanceStatementDate :: MT940Date
    , _intermediateClosingBalanceCurrency :: IsoCurrencyCode
    , _intermediateClosingBalanceAmount :: Amount
    } deriving (Show)

-- | `:62F:`
data FinalClosingBalance = FinalClosingBalance
    { _finalClosingBalanceMark :: CreditDebitMark
    , _finalClosingBalanceStatementDate :: MT940Date
    , _finalClosingBalanceCurrency :: IsoCurrencyCode
    , _finalClosingBalanceAmount :: Amount
    } deriving (Show)

-- | `:64:`
data ClosingAvailableBalance = ClosingAvailableBalance
    { _closingAvailableBalanceMark :: CreditDebitMark
    , _closingAvailableBalanceStatementDate :: MT940Date
    , _closingAvailableBalanceCurrency :: IsoCurrencyCode
    , _closingAvailableBalanceAmount :: Amount
    } deriving (Show)

-- | `:65:`
data FordwardAvailableBalance = FordwardAvailableBalance
    { _forwardAvailableBalanceMark :: CreditDebitMark
    , _forwardAvailableBalanceStatementDate :: MT940Date
    , _forwardAvailableBalanceCurrency :: IsoCurrencyCode
    , _forwardAvailableBalanceAmount :: Amount
    } deriving (Show)

-- | `:86:`
data InformationToAccountOwner = InformationToAccountOwner {} deriving (Show)

-- | MT940 record
data MT940Record = MT940Record
    { _mt940Record20TransactionReferenceNumber :: TransactionReferenceNumber
    , _mt940Record21RelatedReference :: Maybe RelatedReference
    , _mt940Record25AccountIdentification :: AccountIdentification
    , _mt940Record28cStatementNumber :: StatementNumberSeqNumber
    , _mt940Record60aOpeningBalance :: OpeningBalance
    , _mt940Record61StatementLine :: Maybe StatementLine
--, _mt940Record86InformationToAccountOwner :: Maybe InformationToAccountOwner
    , _mt940Record62aClosingBalance :: ClosingBalance
    , _mt940Record64ClosingAvailableBalance :: Maybe ClosingAvailableBalance
    , _mt940Record65FordwardAvailableBalance :: Maybe FordwardAvailableBalance
    , _mt940Record86InformationToAccountOwner :: Maybe InformationToAccountOwner
} deriving (Show)
