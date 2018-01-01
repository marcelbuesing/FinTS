-- | Internationak Bank Account Number
-- | https://en.wikipedia.org/wiki/International_Bank_Account_Number
module FinTS.Data.ISO13616IBAN where

import           Data.Attoparsec.ByteString.Char8 hiding (isDigit)
import           Data.Char
import qualified Data.ISO3166_CountryCodes as CC
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           FinTS.Data.ISO7064CheckDigits
import qualified FinTS.Data.ISO9362BIC as BIC
import           FinTS.Data.SWIFT

data IBAN = IBAN CC.CountryCode CheckDigits BBAN deriving Eq

newtype NationalBankCode = NationalBankCode T.Text deriving (Show, Eq)

newtype AccountNumber = AccountNumber T.Text deriving (Show, Eq)

newtype BranchCode = BranchCode T.Text deriving (Show, Eq)

-- | Country specific check digits e.g. France's Clé RIB
newtype NationalCheckDigits = NationalCheckDigits T.Text deriving (Show, Eq)

-- | Germany Bankleitzahl
-- | https://de.wikipedia.org/wiki/Bankleitzahl
newtype BLZ = BLZ NationalBankCode deriving (Show, Eq)

-- | Clé RIB
newtype RIB = RIB NationalCheckDigits deriving (Show, Eq)

-- | The country specific part of the IBAN
data BBAN =
  -- | Germany
    DE BLZ AccountNumber
  -- | France
  | FR NationalBankCode BranchCode AccountNumber RIB
  -- | Netherlands
  | NL BIC.BankCode AccountNumber
  deriving (Show, Eq) -- TODO fix show

instance Show IBAN where
  show (IBAN c cd ic) = show c <> show cd <> show ic

iban :: Parser IBAN
iban = do
  cc' <- countryCode
  cd' <- CheckDigits . T.pack <$> count 2 digit
  ic' <- ibanCountry cc'
  return $ IBAN cc' cd' ic'

ibanCountry :: CC.CountryCode -> Parser BBAN
ibanCountry CC.NL = nl
ibanCountry _ = error "Country not implemented"

-- a - Upper case Alpha
alphaUpperCase :: Parser Char
alphaUpperCase = satisfy isAsciiUpper

-- n - Numeric Case
-- = digit

isAlphaNumericMixed :: Char -> Bool
isAlphaNumericMixed c = isAsciiUpper c || isAsciiLower c || isDigit c

-- c -- Mixed case alphanumeric char
alphaNumericMixed :: Parser Char
alphaNumericMixed = satisfy isAlphaNumericMixed

de :: Parser BBAN
de = do
  blz' <- BLZ . NationalBankCode . T.pack <$> count 8 digit
  acc' <- AccountNumber . T.pack <$> count 10 digit
  return $ DE blz' acc'

fr :: Parser BBAN
fr = do
  nbc' <- NationalBankCode . T.pack <$> count 5 digit
  brc' <- BranchCode . T.pack <$> count 5 digit
  acc' <- AccountNumber . T.pack <$> count 11 alphaNumericMixed
  rib' <- RIB . NationalCheckDigits . T.pack <$> count 2 digit
  return $ FR nbc' brc' acc' rib'

nl :: Parser BBAN
nl = do
  bc' <- BIC.bankCode
  ac' <- AccountNumber . T.pack <$> count 10 digit
  return $ NL bc' ac'
