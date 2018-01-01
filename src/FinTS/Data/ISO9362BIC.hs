{-# LANGUAGE OverloadedStrings #-}

module FinTS.Data.ISO9362BIC where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           FinTS.Data.SWIFT

newtype BankCode = BankCode T.Text deriving Eq

instance Show BankCode where
  show (BankCode t) = T.unpack t

bankCode :: Parser BankCode
bankCode = BankCode . T.pack <$> count 4 swiftAlpha

newtype LocationCode = LocationCode T.Text deriving Eq

instance Show LocationCode where
  show (LocationCode t) = T.unpack t

locationCode :: Parser LocationCode
locationCode = LocationCode . T.pack <$> count 2 swiftAlphaNumeric

data BranchCode = MainOffice | Branch T.Text deriving (Eq)

instance Show BranchCode where
  show MainOffice = "XXX"
  show (Branch x)= T.unpack x

branchCode :: Parser BranchCode
branchCode =
    option MainOffice (
        try (string "XXX" >> return MainOffice)
    <|> try (Branch . T.pack <$> count 3 swiftAlphaNumeric))

-- | Business Identifier Code (formerly known as Bank Indentifier Code)
data BIC = BIC
  { -- | Institution code or bank code
    _bicBankCode     :: BankCode
    -- | ISO 3166-1 Country Code
  , _bicCountryCode  :: CountryCode
  , _bicLocationCode :: LocationCode
  , _bicBranchCode   :: BranchCode
  } deriving Eq

instance Show BIC where
  show (BIC bc cc lc brc) =
    show bc <> show cc <> show lc <> show brc

bic :: Parser BIC
bic = do
  bankCode'    <- bankCode <?> "BIC BankCode"
  countryCode' <- countryCode <?> "BIC CountryCode"
  locationCode'<- locationCode  <?> "BIC LocationCode"
  branchCode'  <- branchCode  <?> "BIC BranchCode"
  return $ BIC bankCode' countryCode' locationCode' branchCode'
