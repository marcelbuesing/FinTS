module FinTS.Data.IBAN (IBAN(..), IBANGrouping(..), ISO7064CheckDigit(..), iban) where

import           Data.Attoparsec.ByteString.Char8
import           Data.ISO3166_CountryCodes (CountryCode)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           FinTS.Data.SWIFT

newtype ISO7064CheckDigit = ISO7064CheckDigit T.Text deriving (Show, Eq)

data IBAN = IBAN CountryCode ISO7064CheckDigit [IBANGrouping] deriving Eq

instance Show IBAN where
  show (IBAN c cd g) = show c <> show cd <> foldr (\a b -> show a <> b) "" g

iban :: Parser IBAN
iban = try nl

data IBANGrouping =
    IBANDigitGrouping T.Text
  | IBANCharGrouping T.Text
  deriving (Eq)

instance Show IBANGrouping where
  show (IBANDigitGrouping d) = T.unpack $ d
  show (IBANCharGrouping c) = T.unpack $ c

digitGrouping :: Int -> Parser IBANGrouping
digitGrouping x = IBANDigitGrouping . T.pack <$> count x digit

charGrouping :: Int -> Parser IBANGrouping
charGrouping x = IBANCharGrouping . T.pack <$> count x swiftAlpha

nl :: Parser IBAN
nl = do
  cc'   <- countryCode
  cd'   <- ISO7064CheckDigit . T.pack <$> count 2 digit
  grps' <-  sequence [charGrouping 4, digitGrouping 10]
  return $ IBAN cc' cd' grps'
