{-# LANGUAGE OverloadedStrings #-}

-- http://www.hbci-zka.de/english/documents/specification_english/HBCI22eb.pdf
-- http://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Formals_2017-10-06_final_version.pdf
-- https://www.hbci-zka.de/dokumente/spezifikation_deutsch/fintsv3/FinTS_3.0_Security_Sicherheitsverfahren_PINTAN_2017-10-06_final_version.pdf
-- http://martin.hinner.info/bankconvert/swift_mt940_942.pdf

module FinTS.Data.FinTS3 where

import           Control.Lens
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (Default(..))
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           FinTS.Data.MT940
import           Network.HTTP.Client.OpenSSL
import qualified Network.Wreq as Wreq
import           OpenSSL.Session (context)

-- B.4.2 Abgeleitete Formate

class ToFin a where
    toFin :: a -> T.Text

-- | Segment specific identifier. Uppercase with length: ..6.
data SegmentIdentifier =
    ID_HIKAZ
  | ID_HKKAZ
  | ID_HKSAL
  deriving Eq

instance Show SegmentIdentifier where
  show ID_HIKAZ = "HIKAZ"
  show ID_HKKAZ = "HKKAZ"
  show ID_HKSAL = "HKSAL"

instance ToFin SegmentIdentifier where
  toFin ID_HIKAZ = "HIKAZ"
  toFin ID_HKKAZ = "HKKAZ"

-- | Information for uniquely identification of segments within a message.
-- | Segments are numbered ascendingly. Numeration starts with `1`.
-- | version: 1 length: ..3
newtype SegmentNumber = SegmentNumber Int deriving (Eq, Read)

instance Show SegmentNumber where
  show (SegmentNumber n) = show n

instance Default SegmentNumber where
  def = SegmentNumber 1

-- TODO list of predefined segmentversions
-- | Version number for documentation of the segment format.
--   The segment version is incremented for every segment change.
--   version: 1 length: ..3
newtype SegmentVersion = SegmentVersion Int deriving (Eq, Read)

instance Show SegmentVersion where
  show (SegmentVersion v) = show v

segmentVersion :: SegmentIdentifier -> SegmentVersion
segmentVersion ID_HKSAL = SegmentVersion 7
segmentVersion _        = SegmentVersion 0

-- | aka `dat` YYYYMMDD according to ISO 8601
type FinTSDate = Day

-- | aka `tim` hhmmss according to ISO 86
type FinTime = LocalTime

-- | aka `ìd` Alphanumeric, Length: ..30
newtype FinID = FinID T.Text deriving Eq

instance Show FinID where
  show (FinID a) = T.unpack a

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

instance Show SegmentHeader where
  show (SegmentHeader id' num ver ref) =
    show id' <> ":" <> show num <> ":" <> show ver

-- | Alphanumeric Length: ..35
newtype ScrollReference = ScrollReference T.Text deriving Eq

instance Show ScrollReference where
  show (ScrollReference t) = T.unpack t

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
newtype IntCustomerAccount = IntCustomerAccount T.Text deriving Eq

instance Show IntCustomerAccount where
  show (IntCustomerAccount t) = T.unpack t

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
--  G. DataDictionary Kreditinstitutskennung
--  Version 1, format: kik
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

instance Show YesNo where
  show Yes = "J"
  show No  = "N"

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

-- | HBCI Version of interface specification
data HBCIVersion =
  -- | Version 2.0.1 - deprecated
    HBCIVersion_2_0_1
  -- | Version 2.1 - deprecated
  | HBCIVersion_2_1
  -- | Version 2.2 - deprecated
  | HBCIVersion_2_2
  -- | Version 3.0
  | HBCIVersion_3_0
  deriving Eq

-- | According to DataDictionary HBCI-Version
instance Show HBCIVersion where
  show HBCIVersion_2_0_1 = "201"
  show HBCIVersion_2_1   = "210"
  show HBCIVersion_2_2   = "220"
  show HBCIVersion_3_0   = "300"

instance Default HBCIVersion where
  def = HBCIVersion_3_0

-- | Matches a message to FinTS-Dialog.
--   The initial customer message starts with DialogID "0".
--   The bank response message contains the unique DialogID to be used for all the following
--   messages of the dialog. It's the responsibility of the bank to guarantee system wide uniqueness.
newtype DialogID = DialogID FinID deriving Eq

instance Show DialogID where
  show (DialogID a) = show a

instance Default DialogID where
  def = DialogID $ FinID "0"

-- | Combined with the `DialogID` and `CustomerID` the `MessageNumber` can be used to unqiuely
--   identify a message across dialogs. MessageNumbers must increase monotonnically.
--   The message numbering starts with "1". Messages which are not numbered monotonically increasing are
--   rejected on bank and customer side.
newtype MessageNumber = MessageNumber Int deriving (Show, Eq)

instance Default MessageNumber where
  def = MessageNumber 1

data ReferenceMessage =
  -- | Bank message
    ReferenceMessage_M
  -- | Customer message
  | ReferenceMessage_N
  deriving Eq

instance Show ReferenceMessage where
  show ReferenceMessage_M = "M"
  show ReferenceMessage_N = "N"

data ReferenceSegment =
  -- | Bank message
    ReferenceSegment_O
  -- | Customer message
  | ReferenceSegment_N

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
newtype ChallengeHHD_UC = ChallengeHHD_UC BSL.ByteString

data KeyKind = KeyKind_D | KeyKind_S | KeyKind_V

-- | Keynumber of respective key
type KeyNumber = Int

-- | Versionnumber of respective key
type KeyVersion = Int

-- | Version 3
data KeyName = KeyName
  { _keyNameBankIdentification :: BankID
  , _keyNameUserIdentification :: CustomerID
  , _keyNameKeyKind            :: KeyKind
  , _keyNameKeyNumber          :: KeyNumber
  , _keyNameKeyVersion         :: KeyVersion
  }

data Certificate =
  -- | RAH-7, RDH-3, RDH-6. RDH-7 in connection with at least one business transaction
  --   requiring security classification 2, 3 or 4
    Certificate_M
  -- | RAH-9, RDH-1, RDH-5, RDH-8 and RDH-9 in connection with business transactions
  --   requiring security classification 1 or 2
  | Certificate_O
  -- | DDV-1, RAH-10, RDH-2, RDH-10
  | Certificate_N

-- | Segment with Bank or Customer origin
data Segment =
  -- | Header of message
  -- | B.5.2 Nachrichtenkopf
    HNHBK
      { _hnhbkSegmentHeader    :: SegmentHeader
      , _hnhbkSize             :: Int
      , _hnhbkHBCIVersion      :: HBCIVersion
      , _hnhbkDialogID         :: DialogID
      , _hnhbkMessageNumber    :: MessageNumber
      , _hnhbkReferenceMessage :: ReferenceMessage
      }
  -- | Footer of message
  -- | B.5.3 Nachrichtenabschluss
  -- | This segment ends all customer and bank messages
  | HNHBS
     { _hnhbsSegmentHeader :: SegmentHeader
     , _hnhbsMessageNumber :: MessageNumber
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
    , _hktanCustomerAccount :: IntCustomerAccount
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
  -- | C.2.1.2.2 Segmentversion 7 (SEPA) a.)
  | HKSAL
  { _hksalSegmentHeader          :: SegmentHeader
  , _hksalCustomerAccount        :: IntCustomerAccount
  , _hksalAllAccounts            :: YesNo
  , _hksalMaximumNumberOfEntries :: Int
  , _hksalScrollReference        :: ScrollReference
  }
  -- | B.5.1 Signaturkopf Version 4
  | HNSHK
  { _hnshkSegmentHeader               :: SegmentHeader
  , _hnshkSecuriyProfile              :: SecurityProfile
  , _hnshkSecurityFunctionEncoded     :: SecurityFunction
  , _hnshkSecurityControlReference    :: SecurityControlRefernce
  , _hnshkAreaSecurityApplication     :: AreaSecurityApplication
  , _hnshkRoleSecuritySupplierEncoded :: RoleSecuritySupplier
  , _hnshkSecurityIdentification      :: SecurityIdentification
  , _hnshkSecurityReferenceNumber     :: SecurityReferenceNumber
  , _hnshkSecurityDateTime            :: SecurityDateTime
  , _hnshkHashAlgorithm               :: HashAlgorithm
  , _hnshkSignatureAlgorithm          :: SignatureAlgorithm
  , _hnshkKeyName                     :: KeyName
  , _hnhskCertificate                 :: Certificate
  }

instance Show Segment where
  show (HKSAL h ca aa mn sr) = show h <> "+" <>  show ca <> "+" <> show aa <> "'"
  show _ = mempty

 -- | Smart constructor message header
hnhbk :: SegmentIdentifier -> Int -> Segment
hnhbk si size =
  let sh = SegmentHeader si def (segmentVersion si) ReferenceSegment_N
  in HNHBK sh size HBCIVersion_3_0 def def ReferenceMessage_N

askHksal :: String -> Segment -> IO (Either String BS.ByteString)
askHksal url a = do
  let opts = Wreq.defaults & Wreq.manager .~ Left (opensslManagerSettings context)
      content = Base64.encode $ BSC8.pack $ show a
  r <- withOpenSSL $
    Wreq.postWith opts url content
  print r
  return $ Base64.decode $ BSL.toStrict (r ^. Wreq.responseBody)

-- | HNHBK(Header) HNHBS(Footer)

-- sampleHeader :: SegmentHeader
-- sampleHeader = SegmentHeader ID_HKSAL (SegmentNumber 1) (SegmentVersion 3) (ReferenceSegment 1)
