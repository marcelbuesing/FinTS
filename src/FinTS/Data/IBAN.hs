module FinTS.Data.IBAN (IBAN(..), IBANGrouping(..), iban) where

import           Data.Attoparsec.ByteString.Char8
import           Data.ISO3166_CountryCodes (CountryCode)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           FinTS.Data.SWIFT

data IBAN = IBAN CountryCode [IBANGrouping] deriving Eq

instance Show IBAN where
  show (IBAN c g) = (show c) <> foldr (\a b -> show a <> b) "" g

--iban :: Parser IBAN
--iban = IBAN . T.pack <$> count 35 swiftCharacter

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
  grps' <-  sequence [digitGrouping 2, charGrouping 4, digitGrouping 10]
  return $ IBAN cc' grps'
