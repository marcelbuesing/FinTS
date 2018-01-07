{-# LANGUAGE OverloadedStrings #-}

-- http://www.hbci-zka.de/english/documents/specification_english/HBCI22eb.pdf
-- http://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Formals_2017-10-06_final_version.pdf
-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf
-- https://www.hbci-zka.de/dokumente/aenderungen/V3.0/GV_HKTAN-5.pdf

module FinTS.Data.Types where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           FinTS.Data.MT940

-- B.4.2 Abgeleitete Formate

class ToFin a where
    toFin :: a -> T.Text

-- | Segment specific identifier. Uppercase with length: ..6.
data SegmentIdentifier =
    ID_HIKAZ
  | ID_HKKAZ
  deriving (Show, Eq)

instance ToFin SegmentIdentifier where
  toFin ID_HIKAZ = "HIKAZ"
  toFin ID_HKKAZ = "HKKAZ"

-- | Information for uniquely identification of segments within a message.
-- | Segments are numbered ascendingly. Numeration starts with `1`.
-- | version: 1 length: ..3
newtype SegmentNumber = SegmentNumber Int deriving (Show, Eq, Read)

-- | Version number for documentation of the segment format.
-- | version: 1 length: ..3
newtype SegmentVersion = SegmentVersion Int deriving (Show, Eq, Read)

newtype ReferenceSegment = ReferenceSegment Int deriving (Show, Eq, Read)

-- | aka `dat` YYYYMMDD according to ISO 8601
type FinTSDate = Day

-- | aka `tim` hhmmss according to ISO 86
type FinTime = LocalTime

-- | aka `ìd` Alphanumeric, Length: ..30
newtype FinID = FinID T.Text deriving (Show, Eq)

-- Syntax characters

-- | Separation of data elements
sepElem :: Char
sepElem = '+'

($+) :: Char
($+) = sepElem

-- | Separation of data elements within a `DEG`
sepElemDEG :: Char
sepElemDEG = ':'

($:) :: Char
($:) = sepElemDEG

-- | Segment End
segEnd :: Char
segEnd = '\''

($\) :: Char
($\) = segEnd

-- | Escape special characters e.g. `+` becomes `?+`
escape :: Char
escape = '?'

($?) :: Char
($?) = escape

-- | Binary Data
binData :: Char
binData = '@'

($@) :: Char
($@) = binData
------------------

data SegmentHeader = SegmentHeader
  { -- | Segmentkennung
    _segmentHeaderIdentifier :: SegmentIdentifier
  -- | Segmentnummer
  , _segmentHeaderNumber :: SegmentNumber
  -- | Segmentversion
  , _segmentHeaderVersion :: SegmentVersion
  -- | Bezugssegment
  , _segmentHeaderReferenceSegment :: ReferenceSegment
  }

-- | Alphanumeric Length: ..35
newtype ScrollReference = ScrollReference T.Text deriving (Show, Eq)

data AllAccounts =
  -- | All account movements of the customer
  -- | for which he has an access authorisation
  -- | should be reported
    YesAllAccountMovements
  -- | Only the movements of the specified accounts should be reported
  | NoOnlySpecifiedAccountMovements

instance Show AllAccounts where
  show YesAllAccountMovements = "Y"
  show NoOnlySpecifiedAccountMovements = "N"

-- | aka `kti`
-- | Bank account of international client e.g. IBAN
-- | Kontoverbindung international Auftraggeber
newtype IntCustomerAccount = IntCustomerAccount T.Text deriving (Show, Eq)

-- | aka `ktv`
newtype CustomerAccount = CustomerAccount T.Text deriving (Show, Eq)

-- | Customer Identification. Unique per Bank.
-- | G. DataDictionary Kunden-ID
newtype CustomerID = CustomerID T.Text deriving (Show, Eq)

-- | Customer system identification.
-- | Unique identification of customer system.
-- | Guarantees the validity of the signature in combination with the signature-id.
newtype CustomerSystemID = CustomerSystemID T.Text deriving (Show, Eq)

-- | Bank Identification
-- | G. DataDictionary Kreditinstitutskennung
newtype BankID = BankID T.Text deriving (Show, Eq)

-- | Customer System Status.
-- | G. DataDictionary Kundensystem-Status
-- | 0 or 1
data CustomerSystemStatus =
  -- | 0: Customer System Identification not required (HBCI DDV- and chipcard based- process)
    CustomerSystemStatusIDNotRequired
  -- | 1: Customer System Identification required (others, HBCI RAH-/ RDH and PIN/TAN)
  | CustomerSystemStatusIDRequired


data YesNo = Yes | No

-- | 1..4
newtype TANProcess = TANProcess Int deriving (Show, Eq)

newtype SegmentID = SegmentID T.Text deriving (Show, Eq)

-- | HKTAN - Data Dictionary - Auftrags-Hashwert
newtype OrderHashValue = OrderHashValue T.Text deriving (Show, Eq)

-- | Contains reference to order.
-- | HKTAN - Data Dictionary - Auftragsreferenz
newtype OrderReference = OrderReference T.Text deriving (Show, Eq)

newtype TANListNumber = TANListNumber T.Text deriving (Show, Eq)

newtype ChallengeClass = ChallengeClass Int deriving (Show, Eq)

newtype ParameterChallengeClass = ParameterChallengeClass T.Text deriving (Show, Eq)

newtype HBCIVersion = HBCIVersion Int deriving (Show, Eq)

newtype DialogID = DialogID FinID deriving (Show, Eq)

newtype MessageNumber = MessageNumber Int deriving (Show, Eq)

newtype ReferenceMessage = ReferenceMessage T.Text deriving (Show, Eq)

-- | Optional in answer for the sent TAN confirmation number.
-- | Customer has to compare this to the BEN printed on the TAN list
-- | HKTAN - Data Dictionary - BEN
newtype BEN = BEN T.Text deriving (Show, Eq)

-- | Challenge for PIN/TAN process order.
-- | Based on the challenge the customer determines the actual TAN.
-- | The challenge has to be transferred, independent of TAN process version 1 or 2.
-- | HKTAN - Data Dictionary - Challenge, Elementversion
newtype Challenge = Challenge T.Text deriving (Show, Eq)

-- | For use in a Two-Step-Verification process with unidirectional coupling.
-- | In addition to the `Challenge` the data has to be provided e.g. via an optical interface.
newtype ChallengeHHD_UC = ChallengeHHD_UC ByteString

-- | Segment with Bank or Customer origin
data Segment =
  -- | Header of message
  -- | B.5.2 Nachrichtenkopf
    HNHBK
      { _messageHeaderSegmentHeader :: SegmentHeader
      , _messageHeaderSize :: Int
      , _messageHeaderHBCIVersion :: HBCIVersion
      , _messageHeaderDialogeID :: DialogID
      , _messageHeaderMessageNumber :: MessageNumber
      , _messageHeaderReferenceMessage :: ReferenceMessage
      }
  -- | Footer of message
  -- | B.5.3 Nachrichtenabschluss
  -- | This segment ends all customer and bank messages
  | HNHBS
     { _messageFooterSegmentHeader :: SegmentHeader
     , _messageFooterMessageNumber :: MessageNumber
     }
  -- | Account movements during specified period
  -- | Example: HKKAZ:3:5+1234567::280:10020030+N+19960701+19960730'
  -- | Version: 6
  | HKKAZ
    { _hkkazSegmentHeader :: SegmentHeader
    -- | This is the customer account for which account movements are being queried.
    , _hkkazCustomerAccount :: CustomerAccount
    -- | This option can be used for choosing whether only the movements of the
    -- | specified accounts should be reported or all accounts of the customer for
    -- | which he has an access authorisation
    , _hkkazAllAccounts :: AllAccounts
    -- | Used to specify the period for which account movements are desired
    , _hkkazStartDate :: FinTSDate
    -- | Used to specify the period for which account movements are desired
    , _hkkazEndDate :: FinTSDate
    -- | Used to limit the number of account movements returned
    -- | Length: ..4
    , _hkkazMaximumNumberOfEntries :: Int
    -- | Use only if a scroll reference was returned by the bank
    , _hkkazScrollReference :: ScrollReference
    }
  -- | HIKAZ:4:5:3+@362@<MT940>+@102@<MCustomer messageT 942>'
  | HIKAZ
    { _hikazSegmentHeader :: SegmentHeader
    -- | The booked transactions for the specified period.
    -- | One MT940 record is used for each booking day.
    , _hikazBookedTransactions :: [MT940Record]
    , _hikazNonBookedTransactions :: [MT940Record]
    }
  -- | Identification
  -- | C.3.1.2 Segment: Identifikation
  | HKIDN
    { _hkdinSegmentHeader :: SegmentHeader
    , _hkdinBankID :: BankID
    , _hkdinCustomerID :: CustomerID
    , _hkdinCustomerSystemID :: CustomerSystemID
    , _hkdinCustomerSystemStatus :: CustomerSystemStatus
    }
  -- | Request of public key
  -- | C.3.1.4 Segment: Anforderung eines öffentlichen Schlüssels
  | HKTAN
    { _hktanSegmentHeader :: SegmentHeader
    , _hktanTANProcess :: TANProcess
    , _hktanSegmentID :: SegmentID
    , _hktanInternationalBankAccount :: IntCustomerAccount
    , _hktanOrderHashValue :: OrderHashValue
    , _hktanOrderReference :: OrderReference
    , _hktanTANListNumber :: TANListNumber
    , _hktanAnotherTANFollows :: YesNo
    , _hktanCancelOrder :: YesNo
    , _hktanSMSWithdrawalAccount :: IntCustomerAccount
    , _hktanChallengeClass :: ChallengeClass
    , _hktanParameterChallengeClass :: ParameterChallengeClass
    -- | Symbolic name of TAN Data medium e.g. `TAN-Generator`
    , _hktanTitleTANDataMedium :: T.Text
    }
  | HITAN
    { _hitanSegmentHeader :: SegmentHeader
    , _hitanTANProcess :: TANProcess
    , _hitanOrderHashValue :: OrderHashValue
    , _hitanOrderReference :: OrderReference
    , _hitanChallenge :: Challenge
    , _hitanChallengeHHD_UC :: ChallengeHHD_UC
    , _hitanDateTimeOfExpiry :: FinTime
    , _hitanTANListNumber :: TANListNumber
    , _hitanBEN :: BEN
    -- | Symbolic name of TAN Data medium e.g. `TAN-Generator`
    , _hitanTitleTANDataMedium :: T.Text
    }




