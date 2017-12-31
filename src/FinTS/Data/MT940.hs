{-# LANGUAGE OverloadedStrings #-}

-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf
-- https://web.archive.org/web/20160725042101/http://www.societegenerale.rs/fileadmin/template/main/pdf/SGS%20MT940.pdf
-- https://www.db-bankline.deutsche-bank.com/download/MT940_Deutschland_Structure2002.pdf
-- http://www.sepaforcorporates.com/swift-for-corporates/account-statement-mt940-file-format-overview/
-- https://www.ibm.com/support/knowledgecenter/en/SSRH46_3.0.0/fxhmapr2016wtxswibtxtmt940.html
-- https://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Messages_Finanzdatenformate_2010-08-06_final_version.pdf

module FinTS.Data.MT940 where

import           Control.Applicative ((<|>), optional)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString as BS hiding (count)
import qualified Data.ByteString.Char8 as B
import           Data.Currency as Currency
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian)
import           Data.Time.Format (parseTimeM)
import           Data.Tuple.Curry (uncurryN)

import           FinTS.Data.SWIFT
import           FinTS.Data.IBAN

type MT940Date = Day

mt940Date :: Parser MT940Date
mt940Date = do
  yy <- year . read <$> count 2 digit
  mm <- read <$> count 2 digit
  dd <- read <$> count 2 digit
  return $ fromGregorian yy mm dd
  where year x = if x > 90 -- regret this 2091
                 then x + 1900
                 else x + 2000

currency :: Parser Currency.Alpha
currency = read <$> count 3 swiftAlpha

digitOrAlpha :: Parser Char
digitOrAlpha = satisfy (\x -> isDigit x || isUpperCase x)
    where isUpperCase c = (c >= 'A' && c <= 'Z')

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

bankReference :: Parser BankReference
bankReference = do
  _ <- string "//"
  BankReference . T.pack <$> count 16 swiftCharacter

newtype CustomerReference = CustomerReference T.Text deriving (Show)

customerReference :: Parser CustomerReference
customerReference = CustomerReference . T.pack <$> count 16 swiftCharacter

newtype FundsCode = FundsCode Char deriving (Show)

fundsCode :: Parser FundsCode
fundsCode = FundsCode <$> swiftCharacter

newtype Amount = Amount Double deriving (Show, Eq, Ord)



-- | double with comma
double' :: Parser Double
double' = do
    d <- decimal
    f <- fmap (fromMaybe 0) $ optional $ do
        _ <- char ','
        s <- takeWhile1 isDigit
        let ln = B.length s
            v = fromIntegral $ getInt s
        return (v / (10 ^ ln))
    return $! (fromIntegral d + f)
    where getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0

amount :: Parser Amount
amount = Amount <$> double'

newtype StatementNumber = StatementNumber Integer deriving (Show, Eq, Read)
newtype SeqNumber = SeqNumber Integer deriving (Show, Eq, Ord, Read)


data BICBranchCode = MainOffice | OtherBranch T.Text deriving (Eq)

instance Show BICBranchCode where
  show MainOffice = "XXX"
  show (OtherBranch x)= T.unpack x

bicBranchCode :: Parser BICBranchCode
bicBranchCode = do
        (string "XXX" >> return MainOffice)
    <|> (OtherBranch . T.pack <$> count 3 digitOrAlpha)

newtype BICBankCode = BICBankCode T.Text deriving (Eq)

instance Show BICBankCode where
  show (BICBankCode x) = T.unpack x

newtype BICLocationCode = BICLocationCode T.Text deriving (Eq)

instance Show BICLocationCode where
  show (BICLocationCode x) = T.unpack x

-- | Bank Identifier Code
data BIC = BIC
  { _bicBankCode :: BICBankCode
  , _bicCountryCode :: CountryCode
  , _bicLocationCode :: BICLocationCode
  , _bicBranchCode :: Maybe BICBranchCode
  } deriving (Eq)

instance Show BIC where
  show (BIC bc cc lc brc) = show bc <> show cc <> show lc <> maybe mempty show brc

-- TODO FIX
bic :: Parser BIC
bic = do
    bankCode     <- BICBankCode . T.pack <$> count 4 swiftAlpha <?> "BIC BankCode"
    countryCode'  <- countryCode
    locationCode <- BICLocationCode . T.pack<$> count 2 digitOrAlpha <?> "BIC LocationCode"
    branchCode   <- option Nothing $ Just <$> (bicBranchCode <?> "BIC BranchCode")
    return $ BIC bankCode countryCode' locationCode branchCode


-- | `:20:`
data TransactionReferenceNumber = TransactionReferenceNumber
    { _transactionReferenceNumber :: T.Text
    }  deriving (Show, Eq, Read)

transactionReferenceNumber :: Parser TransactionReferenceNumber
transactionReferenceNumber = do
    _ <- string ":20:" <?> ":20: TransactionReferenceNumber Prefix"
    n <- T.pack <$> count 16 swiftCharacter <?> ":20: TransactionReferenceNumber"
    return $ TransactionReferenceNumber n

-- | `:21:`
data RelatedReference = RelatedReference
    { _relatedReference :: T.Text
    } deriving (Show)

-- | `:25:`
newtype AccountIdentification = AccountIdentification IBAN deriving (Show, Eq)

accountIdentification :: Parser AccountIdentification
accountIdentification = do
    _ <- ":25:" <?> ":25: Account Identification Prefix"
    id' <- iban <?> ":25: Account Identification"
    return $ AccountIdentification id'

-- | `:25P:`
data AccountIdentificationIdentifierCode = AccountIdentificationIdentifierCode
    { _accountIdentificationIdentifierCodeAccount :: IBAN
    , _accountIdentificationIdentifierCodeIdentifierCode :: BIC
    }

accountIdentificationIdentifierCode :: Parser AccountIdentificationIdentifierCode
accountIdentificationIdentifierCode = do
    _   <- ":25P:" <?> ":25P: AccountIdentificationIdentifierCode Prefix"
    id'  <- iban <?> ":25P: Account Identification"
    _   <- endOfLine
    bic' <- bic <?> ":25P: Identifier Code (BIC)"
    return $ AccountIdentificationIdentifierCode id' bic'

-- | `:28C:`
data StatementNumberSeqNumber = StatementNumberSeqNumber
    { _statementNumber :: StatementNumber
    , _seqNumber       :: Maybe SeqNumber
    } deriving (Show, Eq)

statementNumberSeqNumber :: Parser StatementNumberSeqNumber
statementNumberSeqNumber = do
    _ <- string ":28C:" <?> ":28C: StatementNumberSeqNumber Prefix"
    stn <- StatementNumber <$> maxFiveDigitDecimal <?> ":28C: StatementNumber"
    sqn <- option Nothing $ char '/' *>  ((Just . SeqNumber) <$> maxFiveDigitDecimal <?> ":28C: SequenceNumber")
    return $ StatementNumberSeqNumber stn sqn
     -- TODO figure out a better to avoid backtracking for digits < 5
    where maxFiveDigitDecimal = read <$> count 5 digit <|> decimal

-- | `:60a:`
data OpeningBalance = OpeningBalance
    { _openingBalanceMark :: CreditDebitMark
    , _openingBalanceStatementDate :: MT940Date
    , _openingBalanceCurrency :: Currency.Alpha
    , _openingBalanceAmount :: Amount
    } deriving (Show)

openingBalance :: Parser OpeningBalance
openingBalance = do
  _  <- string ":60a:"
  cd <- creditDebit <?> ":60a: CreditDebit"
  d  <- mt940Date <?> ":60a: Date"
  cc <- currency <?> ":60a: Currency"
  a  <- amount <?> ":60a: Amount"
  return $ OpeningBalance cd d cc a

-- | `:60F:`
data FirstOpeningBalance = FirstOpeningBalance
    { _firstOpeningBalanceMark :: CreditDebitMark
    , _firstOpeningBalanceStatementDate :: MT940Date
    , _firstOpeningBalanceCurrency :: Currency.Alpha
    , _firstOpeningBalanceAmount :: Amount
    } deriving (Show)

firstOpeningBalance :: Parser FirstOpeningBalance
firstOpeningBalance = do
  _  <- string ":60F:"
  cd <- creditDebit <?> ":60F: CreditDebit"
  d  <- mt940Date <?> ":60F: Date"
  cc <- currency <?> ":60F: Currency"
  a  <- amount <?> ":60F: Amount"
  return $ FirstOpeningBalance cd d cc a

-- | `:60M:`
data IntermediateBalance = IntermediateBalance
    { _intermediateBalanceMark :: CreditDebitMark
    , _intermediateBalanceStatementDate :: MT940Date
    , _intermediateBalanceCurrency :: Currency.Alpha
    , _intermediateBalanceAmount :: Amount
    } deriving (Show)

intermediateOpeningBalance :: Parser IntermediateBalance
intermediateOpeningBalance = do
  _  <- string ":60M:"
  cd <- creditDebit <?> ":60M: CreditDebit"
  d  <- mt940Date <?> ":60M: Date"
  cc <- currency <?> ":60M: Currency"
  a  <- amount <?> ":60M: Amount"
  return $ IntermediateBalance cd d cc a

-- | `:61:`
data StatementLine = StatementLine
    { _statementLineValueDate :: MT940Date
    , _statementLineEntryDate :: Maybe MT940Date
    , _statementLineMark :: CreditDebitMark
    , _statementLineFundsCode :: Maybe FundsCode
    , _statementLineAmount :: Amount
    , _statementLineTransactionTypeIdentificationCode :: TransactionTypeIdentCode
    , _statementLineCustomerReference :: CustomerReference
    , _statementLineBankReference :: Maybe BankReference
    , _statementLineSupplementaryDetails :: T.Text
    } deriving (Show)

statementLine :: Parser StatementLine
statementLine = do
  _         <- string ":61:"
  valueD <- mt940Date
  entryD <- option Nothing (Just <$> mt940Date)
  cd     <- creditDebit
  fc     <- option Nothing (Just <$> fundsCode)
  amount <- amount
  ttic   <- transactionTypeIdentCode
  cr     <- customerReference
  br     <- option Nothing (Just <$> bankReference)
  sd     <- T.pack <$> count 34 swiftCharacter
  return $ StatementLine valueD entryD cd fc amount ttic cr br sd

balance :: B.ByteString -> Parser (CreditDebitMark, MT940Date, Currency.Alpha, Amount)
balance tag = do
  _  <- string tag
  cd <- creditDebit <?> B.unpack tag <> " CreditDebit"
  d  <- mt940Date   <?> B.unpack tag <> " Date"
  cc <- currency    <?> B.unpack tag <> " Currency"
  a  <- amount      <?> B.unpack tag <> " Amount"
  return $ (cd, d, cc, a)

-- | `:62:`
data ClosingBalance = ClosingBalance
    { _closingBalanceMark :: CreditDebitMark
    , _closingBalanceStatementDate :: MT940Date
    , _closingBalanceCurrency :: Currency.Alpha
    , _closingBalanceAmount :: Amount
    } deriving (Show)

closingBalance :: Parser ClosingBalance
closingBalance = (uncurryN ClosingBalance) <$> (balance ":62:")

-- | `:62M:`
data IntermediateClosingBalance = IntermediateClosingBalance
    { _intermediateClosingBalanceMark :: CreditDebitMark
    , _intermediateClosingBalanceStatementDate :: MT940Date
    , _intermediateClosingBalanceCurrency :: Currency.Alpha
    , _intermediateClosingBalanceAmount :: Amount
    } deriving (Show)

intermediateClosingBalance :: Parser IntermediateClosingBalance
intermediateClosingBalance = (uncurryN IntermediateClosingBalance) <$> (balance ":62M:")

-- | `:62F:`
data FinalClosingBalance = FinalClosingBalance
    { _finalClosingBalanceMark :: CreditDebitMark
    , _finalClosingBalanceStatementDate :: MT940Date
    , _finalClosingBalanceCurrency :: Currency.Alpha
    , _finalClosingBalanceAmount :: Amount
    } deriving (Show)

finalClosingBalance :: Parser FinalClosingBalance
finalClosingBalance = (uncurryN FinalClosingBalance) <$> (balance ":62F:")

-- | `:64:`
data ClosingAvailableBalance = ClosingAvailableBalance
    { _closingAvailableBalanceMark :: CreditDebitMark
    , _closingAvailableBalanceStatementDate :: MT940Date
    , _closingAvailableBalanceCurrency :: Currency.Alpha
    , _closingAvailableBalanceAmount :: Amount
    } deriving (Show)

closingAvailableBalance :: Parser ClosingAvailableBalance
closingAvailableBalance = (uncurryN ClosingAvailableBalance) <$> (balance ":64:")

-- | `:65:`
data ForwardAvailableBalance = ForwardAvailableBalance
    { _forwardAvailableBalanceMark :: CreditDebitMark
    , _forwardAvailableBalanceStatementDate :: MT940Date
    , _forwardAvailableBalanceCurrency :: Currency.Alpha
    , _forwardAvailableBalanceAmount :: Amount
    } deriving (Show)

forwardAvailableBalance :: Parser ForwardAvailableBalance
forwardAvailableBalance = (uncurryN ForwardAvailableBalance) <$> (balance ":65:")

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
    , _mt940Record65FordwardAvailableBalance :: Maybe ForwardAvailableBalance
    , _mt940Record86InformationToAccountOwner :: Maybe InformationToAccountOwner
} deriving (Show)
