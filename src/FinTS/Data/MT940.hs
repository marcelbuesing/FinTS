{-# LANGUAGE OverloadedStrings #-}

-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf
-- https://web.archive.org/web/20160725042101/http://www.societegenerale.rs/fileadmin/template/main/pdf/SGS%20MT940.pdf
-- https://www.db-bankline.deutsche-bank.com/download/MT940_Deutschland_Structure2002.pdf
-- http://www.sepaforcorporates.com/swift-for-corporates/account-statement-mt940-file-format-overview/
-- https://www.ibm.com/support/knowledgecenter/en/SSRH46_3.0.0/fxhmapr2016wtxswibtxtmt940.html
-- https://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Messages_Finanzdatenformate_2010-08-06_final_version.pdf

module FinTS.Data.MT940 where

import           Control.Applicative (Alternative(..), (<|>), optional)
import           Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString as BS hiding (count)
import qualified Data.ByteString.Char8 as B
import           Data.Currency as Currency
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian, toGregorian)
import           Data.Tuple.Curry (uncurryN)

import           FinTS.Data.SWIFT
import           FinTS.Data.ISO9362_BIC
import           FinTS.Data.ISO13616_IBAN

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

-- A date missing the year
mt940DateNoYear :: Integer -> Parser MT940Date
mt940DateNoYear year' = do
  mm <- read <$> count 2 digit
  dd <- read <$> count 2 digit
  return $ fromGregorian year' mm dd

currency :: Parser Currency.Alpha
currency = read <$> count 3 swiftAlpha

digitOrAlpha :: Parser Char
digitOrAlpha = satisfy (\x -> isDigit x || isUpperCase x)
    where isUpperCase c = (c >= 'A' && c <= 'Z')


maxCount :: Int -> Parser a -> Parser [a]
maxCount c p = try (count c p) <|> many' p

maxCount1 :: Int -> Parser a -> Parser [a]
maxCount1 c p = try (count c p) <|> many1 p

data CreditDebitMark = Credit | Debit deriving (Show, Eq)

creditDebit :: Parser CreditDebitMark
creditDebit =
      (char 'C' >> return Credit)
  <|> (char 'D' >> return Debit)

data TransactionTypeIdentCode = N | F deriving (Show, Eq)

transactionTypeIdentCode :: Parser TransactionTypeIdentCode
transactionTypeIdentCode =
      (char 'N' >> return N)
  <|> (char 'F' >> return F)

newtype BankReference = BankReference T.Text deriving (Show, Eq)

bankReference :: Parser BankReference
bankReference = do
  _ <- string "//"
  BankReference . T.pack <$> maxCount1 16 swiftCharacter

newtype CustomerReference = CustomerReference T.Text deriving (Show, Eq)

customerReference :: Parser CustomerReference
customerReference = CustomerReference . T.pack <$> maxCount1 16 (satisfy (\c -> c /= '\o57' && isSwiftCharacter c))

newtype FundsCode = FundsCode Char deriving (Show, Eq)

fundsCode :: Parser FundsCode
fundsCode = FundsCode <$> swiftAlpha

newtype Amount = Amount Double deriving (Show, Eq, Ord)

-- | double with comma
double' :: Parser Double
double' = do
    d <- decimal
    f <- fmap (fromMaybe 0) $ optional $ do
        _ <- char ','
        s <- Atto.takeWhile isDigit
        let ln = B.length s
            v = fromIntegral $ getInt s
        return (v / (10 ^ ln))
    return $! (fromIntegral d + f)
    where getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0

amount :: Parser Amount
amount = Amount <$> double'

newtype StatementNumber = StatementNumber Integer deriving (Show, Eq, Read)
newtype SeqNumber = SeqNumber Integer deriving (Show, Eq, Ord, Read)

-- | `:20:`
data TransactionReferenceNumber = TransactionReferenceNumber
    { _transactionReferenceNumber :: T.Text
    }  deriving (Show, Eq, Read)

transactionReferenceNumber :: Parser TransactionReferenceNumber
transactionReferenceNumber = do
    _ <- string ":20:" <?> ":20: TransactionReferenceNumber Prefix"
    n <- T.pack <$> maxCount1 16 swiftCharacter <?> ":20: TransactionReferenceNumber"
    return $ TransactionReferenceNumber n

-- | `:21:`
data RelatedReference = RelatedReference
    { _relatedReference :: T.Text
    } deriving (Show, Eq)

relatedReference :: Parser RelatedReference
relatedReference = do
  _ <- ":21:" <?> ":21: RelatedReference prefix"
  n <- T.pack <$> maxCount1 35 swiftCharacter
  pure $ RelatedReference n

-- | `:25:`
data AccountIdentification =
    AccountIdentificationIBAN IBAN
  | AccountIdentificationBankSpecific T.Text
  deriving (Show, Eq)

accountIdentification :: Parser AccountIdentification
accountIdentification = do
    _ <- ":25:" <?> ":25: Account Identification Prefix"
    let iban' = AccountIdentificationIBAN <$> iban <?> ":25: Account Identification"
        bank' = AccountIdentificationBankSpecific . T.pack<$> maxCount1 35 swiftCharacter
    id' <- try iban' <|> bank'
    return id'

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
    sqn <- option Nothing $ char '/' *> ((Just . SeqNumber) <$> maxFiveDigitDecimal <?> ":28C: SequenceNumber")
    return $ StatementNumberSeqNumber stn sqn
     -- TODO figure out a better to avoid backtracking for digits < 5
    where maxFiveDigitDecimal = read <$> maxCount1 5 digit

-- | `:60a:`
data OpeningBalance =
  -- | `:60F:`
    FirstOpeningBalance
    { _firstOpeningBalanceMark :: CreditDebitMark
    , _firstOpeningBalanceStatementDate :: MT940Date
    , _firstOpeningBalanceCurrency :: Currency.Alpha
    , _firstOpeningBalanceAmount :: Amount
    } |
  -- | `:60M:`
    IntermediateOpeningBalance
    { _intermediateOpeningBalanceMark :: CreditDebitMark
    , _intermediateOpeningBalanceStatementDate :: MT940Date
    , _intermediateOpeningBalanceCurrency :: Currency.Alpha
    , _intermediateOpeningBalanceAmount :: Amount
    } deriving (Show, Eq)

openingBalance :: Parser OpeningBalance
openingBalance = try intermediateOpeningBalance <|> firstOpeningBalance

firstOpeningBalance :: Parser OpeningBalance
firstOpeningBalance = do
  _  <- string ":60F:"
  cd <- creditDebit <?> ":60F: CreditDebit"
  d  <- mt940Date <?> ":60F: Date"
  cc <- currency <?> ":60F: Currency"
  a  <- amount <?> ":60F: Amount"
  return $ FirstOpeningBalance cd d cc a

intermediateOpeningBalance :: Parser OpeningBalance
intermediateOpeningBalance = do
  _  <- string ":60M:"
  cd <- creditDebit <?> ":60M: CreditDebit"
  d  <- mt940Date <?> ":60M: Date"
  cc <- currency <?> ":60M: Currency"
  a  <- amount <?> ":60M: Amount"
  return $ IntermediateOpeningBalance cd d cc a

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
    , _statementLineSupplementaryDetails :: Maybe T.Text
    } deriving (Show, Eq)

statementLine :: Parser StatementLine
statementLine = do
  _         <- string ":61:"
  valueD  <- mt940Date <?> ":61: ValueDate"
  let (year',_ , _ ) = toGregorian valueD
  entryD  <- option Nothing (Just <$> mt940DateNoYear year') <?> ":61: EntryDate"
  cd      <- creditDebit <?> ":61: Mark"
  fc      <- option Nothing (Just <$> fundsCode) <?> ":61: FundsCode"
  amount' <- amount <?> ":61: Amount"
  ttic    <- transactionTypeIdentCode <?> ":61: TransactionType"
  cr      <- customerReference <?> ":61: Customer Ref"
  br      <- option Nothing (Just <$> bankReference) <?> ":61: Bank Ref"
  sd      <- option Nothing (Just . T.pack <$> maxCount1 34 swiftCharacter) <?> ":61: SupplementaryDetails"
  pure $ StatementLine valueD entryD cd fc amount' ttic cr br sd

balance :: B.ByteString -> Parser (CreditDebitMark, MT940Date, Currency.Alpha, Amount)
balance tag = do
  _  <- string tag
  cd <- creditDebit <?> B.unpack tag <> " CreditDebit"
  d  <- mt940Date   <?> B.unpack tag <> " Date"
  cc <- currency    <?> B.unpack tag <> " Currency"
  a  <- amount      <?> B.unpack tag <> " Amount"
  pure (cd, d, cc, a)

-- | `:62a:`
data ClosingBalance =
  -- | `:62M:`
    IntermediateClosingBalance
    { _intermediateClosingBalanceMark :: CreditDebitMark
    , _intermediateClosingBalanceStatementDate :: MT940Date
    , _intermediateClosingBalanceCurrency :: Currency.Alpha
    , _intermediateClosingBalanceAmount :: Amount
    } |
  -- | `:62F:`
    FinalClosingBalance
    { _finalClosingBalanceMark :: CreditDebitMark
    , _finalClosingBalanceStatementDate :: MT940Date
    , _finalClosingBalanceCurrency :: Currency.Alpha
    , _finalClosingBalanceAmount :: Amount
    } deriving (Show, Eq)

closingBalance :: Parser ClosingBalance
closingBalance = try intermediateClosingBalance <|> finalClosingBalance

intermediateClosingBalance :: Parser ClosingBalance
intermediateClosingBalance = (uncurryN IntermediateClosingBalance) <$> (balance ":62M:")

finalClosingBalance :: Parser ClosingBalance
finalClosingBalance = (uncurryN FinalClosingBalance) <$> (balance ":62F:")

-- | `:64:`
data ClosingAvailableBalance = ClosingAvailableBalance
    { _closingAvailableBalanceMark :: CreditDebitMark
    , _closingAvailableBalanceStatementDate :: MT940Date
    , _closingAvailableBalanceCurrency :: Currency.Alpha
    , _closingAvailableBalanceAmount :: Amount
    } deriving (Show, Eq)

closingAvailableBalance :: Parser ClosingAvailableBalance
closingAvailableBalance = (uncurryN ClosingAvailableBalance) <$> (balance ":64:")

-- | `:65:`
data ForwardAvailableBalance = ForwardAvailableBalance
    { _forwardAvailableBalanceMark :: CreditDebitMark
    , _forwardAvailableBalanceStatementDate :: MT940Date
    , _forwardAvailableBalanceCurrency :: Currency.Alpha
    , _forwardAvailableBalanceAmount :: Amount
    } deriving (Show, Eq)

forwardAvailableBalance :: Parser ForwardAvailableBalance
forwardAvailableBalance = (uncurryN ForwardAvailableBalance) <$> (balance ":65:")

-- | `:86:`
newtype InformationToAccountOwner = InformationToAccountOwner [T.Text] deriving (Eq, Show)

crlf :: Parser Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"

informationToAccountOwner :: Parser InformationToAccountOwner
informationToAccountOwner = do
  _ <- string ":86:" <?> ":86: InformationToAccountOwner Prefix"
  hd' <- T.pack <$> maxCount1 65 swiftCharacter
  let allowedChar = satisfy (\c -> c /= ':' && isSwiftCharacter c)
  tl' <- maxCount 5 $ T.pack <$> (crlf *> maxCount1 65 allowedChar)
  pure $ InformationToAccountOwner (hd' : tl')

data Statement = Statement
  { _statement61StatementLine :: StatementLine
  , _statment86InformationToAccountOwner :: Maybe InformationToAccountOwner
  } deriving (Show, Eq)

statement :: Parser Statement
statement = do
  sl' <- statementLine
  ia' <- option Nothing $ Just <$> (crlf *> informationToAccountOwner)
  pure $ Statement sl' ia'

-- | MT940 record
data MT940Record = MT940Record
    { _mt940Record20TransactionReferenceNumber :: TransactionReferenceNumber
    , _mt940Record21RelatedReference           :: Maybe RelatedReference
    , _mt940Record25AccountIdentification      :: AccountIdentification
    , _mt940Record28cStatementNumber           :: StatementNumberSeqNumber
--    , _mt940Record60aOpeningBalance :: OpeningBalance
    , _mt940Record60aOpeningBalance            :: OpeningBalance
    , _mt940RecordStatement                    :: [Statement]
    , _mt940Record62aClosingBalance            :: ClosingBalance
    , _mt940Record64ClosingAvailableBalance    :: Maybe ClosingAvailableBalance
    , _mt940Record65FordwardAvailableBalance   :: Maybe ForwardAvailableBalance
} deriving (Show, Eq)


mt940Record :: Parser MT940Record
mt940Record = do
  a <- transactionReferenceNumber <?> "MT940 TransactionReferenceNumber"
  b <- option Nothing (Just <$> (crlf *>  relatedReference)) <?> "MT940 Related Reference"
  c <- crlf *> accountIdentification <?> "MT940 AccountIdentification"
  d <- crlf *> statementNumberSeqNumber  <?> "MT940 StatementNumber"
  e <- crlf *> openingBalance <?> "MT940 Opening Balance"
  f <- many (try (crlf *> statement))  <?> "MT940 Statement"
  g <- crlf *> closingBalance  <?> "MT940 Closing Balance"
  h <- option Nothing (Just <$> (crlf *> closingAvailableBalance)) <?> "MT940 Closing Available Balance"
  i <- option Nothing (Just <$> (crlf *> forwardAvailableBalance)) <?> "MT940 Forward Available Balance"
  pure $ MT940Record a b c d e f g h i
