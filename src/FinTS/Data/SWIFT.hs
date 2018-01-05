module FinTS.Data.SWIFT where

import           Data.Attoparsec.ByteString.Char8
import           Data.ISO3166_CountryCodes (CountryCode)

-- | https://www2.swift.com/uhbonline/books/public/en_uk/usgi_20160722/con_31519.htm
-- | Appendix Supported Characters - https://deutschebank.nl/nl/docs/MT94042_EN.pdf
isSwiftCharacter :: Char -> Bool
isSwiftCharacter c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') ||
    (c == '\o40')  || -- ` ` space
    (c == '\o47')  || -- `'`
    (c == '\o50')  || -- `(`
    (c == '\o51')  || -- `)`
    (c == '\o53')  || -- `+`
    (c == '\o54')  || -- `,`
    (c == '\o55')  || -- `-`
    (c == '\o56')  || -- `.`
    (c == '\o57')  || -- `/`
    (c == '\o72')  || -- `:`
    (c == '\o77')  || -- `?`
    (c == '\o173') || -- `{`
    (c == '\o175')    -- `}`

swiftCharacter :: Parser Char
swiftCharacter = satisfy isSwiftCharacter <?> "SwiftCharacter"

isSwiftAlpha :: Char -> Bool
isSwiftAlpha c = c >= 'A' && c <= 'Z'

swiftAlpha :: Parser Char
swiftAlpha = satisfy isSwiftAlpha <?> "SwiftAlpha"

isSwiftAlphaNumeric :: Char -> Bool
isSwiftAlphaNumeric c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

swiftAlphaNumeric :: Parser Char
swiftAlphaNumeric = satisfy isSwiftAlphaNumeric <?> "SwiftAlphaNumeric"

countryCode :: Parser CountryCode
countryCode = read <$> count 2 swiftAlpha <?> "CountryCode"
