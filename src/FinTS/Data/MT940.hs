{-# LANGUAGE OverloadedStrings #-}

-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf
-- https://web.archive.org/web/20160725042101/http://www.societegenerale.rs/fileadmin/template/main/pdf/SGS%20MT940.pdf
-- https://www.db-bankline.deutsche-bank.com/download/MT940_Deutschland_Structure2002.pdf
-- http://www.sepaforcorporates.com/swift-for-corporates/account-statement-mt940-file-format-overview/

module FinTS.Data.MT940 where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM)

type MT940Date = Day

data CreditDebit = Credit | Debit deriving (Show)

data TransactionTypeIdentCode = N | F deriving (Show)

--newtype TransactionRefN = TransactionRefN T.Text
newtype BankReference = BankReference T.Text deriving (Show)
newtype TransactionNumber = TransactionNumber T.Text deriving (Show)
newtype IsoCurrencyCode = IsoCurrencyCode T.Text deriving (Show)
newtype Amount = Amount Double deriving (Show)

-- | `:20:`
data TransactionReferenceNumber = TransactionReferenceNumber
    { _transactionReferenceNumber :: T.Text 
    }  deriving (Show) 

-- | `:21:`
data RelatedReference = RelatedReference
    { _relatedReference :: T.Text 
    } deriving (Show) 

-- | `:25:`
data AccountIdentification = AccountIdentification {} deriving (Show) 

-- | `:28C:`
data StatementNumberSeqNumber = StatementNumberSeqNumber {} deriving (Show) 

-- | `:60:`
data OpeningBalance = OpeningBalance
    { _openingBalanceMark :: CreditDebit
    , _openingBalanceStatementDate :: MT940Date
    , _openingBalanceCurrency :: IsoCurrencyCode
    , _openingBalanceAmount :: Amount
    } deriving (Show) 

-- | `:61:`
data StatementLine = StatementLine
    { _statementLineValueDate :: MT940Date
    , _statementLineMark :: CreditDebit
    , _statementLineAmount :: Amount
    , _statementLineTransactionTypeIdentificationCode :: TransactionTypeIdentCode
    , _statementLineTransactionNumber :: TransactionNumber
    , _statementLineBankReference :: BankReference
    , _statementLineSupplementaryDetails :: T.Text
    } deriving (Show) 

-- | `:86:`
data InformationToAccountOwner = InformationToAccountOwner {} deriving (Show) 

-- | `:62:`
data ClosingBalance = ClosingBalance
    { _closingBalanceMark :: CreditDebit
    , _closingBalanceStatementDate :: MT940Date
    , _closingBalanceCurrency :: IsoCurrencyCode
    , _closingBalanceAmount :: Amount
    } deriving (Show) 

-- | `:64:`
data ClosingAvailableBalance = ClosingAvailableBalance
    { _closingAvailableBalanceMark :: CreditDebit
    , _closingAvailableBalanceStatementDate :: MT940Date
    , _closingAvailableBalanceCurrency :: IsoCurrencyCode
    , _closingAvailableBalanceAmount :: Amount
    } deriving (Show) 

-- | `:64:`
data FordwardAvailableBalance = FordwardAvailableBalance
    { _forwardAvailableBalanceMark :: CreditDebit
    , _forwardAvailableBalanceStatementDate :: MT940Date
    , _forwardAvailableBalanceCurrency :: IsoCurrencyCode
    , _forwardAvailableBalanceAmount :: Amount
    } deriving (Show) 

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
