module FinTS.Data.ISO7064_CheckDigits where

import qualified Data.Text as T

newtype CheckDigits = CheckDigits T.Text deriving (Show, Eq)

-- TODO add fns for actual verification
