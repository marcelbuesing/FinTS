{-# LANGUAGE OverloadedStrings #-}

-- http://www.hbci-zka.de/english/documents/specification_english/HBCI22eb.pdf
-- http://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Formals_2017-10-06_final_version.pdf
-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf

module FinTS.Data.Types where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T

class ToFin a where
    toFin :: a -> T.Text

data ClientRequest =
    -- | Account movements during specified period
    -- | Example: HKKAZ:3:5+1234567::280:10020030+N+19960701+19960730'
    HKKAZ
    { _hkkazSegmentHeader :: SegmentHeader
    -- | This is the customer account for which account movements are being queried.
    , _hkkazCustomerAccount :: Acc
    -- | This option can be used for choosing whether only the movements of the
    -- | specified accounts should be reported or all accounts of the customer for
    -- | which he has an access authorisation
    , _hkkazAllAccounts :: Yn
    -- | Used to specify the period for which account movements are desired
    , _hkkazStartDate :: Dat
    -- | Used to specify the period for which account movements are desired
    , _hkkazEndDate :: Dat
    -- | Used to limit the number of account movements returned
    , _hkkazMaximumNumberOfEntries :: Num
    -- | Use only if a scroll reference was returned by the bank
    , _hkkazScrollReference :: An
    }



-- |
data ServerResponse =
    -- | HIKAZ:4:5:3+@362@<MT940>+@102@<MT 942>'
    HIKAZ
    { _hikazSegmentHeader :: SegmentHeader
    -- | The booked transactions for the specified period.
    -- | One MT940 record is used for each booking day.
    , _hikazBookedTransactions :: [MT940Record]
    , _hikazNonBookedTransactions :: [MT940Record]
    }

serverResponse :: Parser ServerResponse
