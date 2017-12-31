module FinTS.Data.IBAN where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import           FinTS.Data.SWIFT

newtype IBAN = IBAN T.Text deriving (Show, Eq)

iban :: Parser IBAN
iban = IBAN . T.pack <$> count 35 swiftCharacter
