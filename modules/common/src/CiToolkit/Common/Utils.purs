module CiToolkit.Common.Utils
  ( unsafeCharFromCharCode
  , unsafeDate
  , unsafeDateTimeFromSeconds
  , unsafeInstantFromSeconds
  , unsafeNonEmptyString
  , unsafeTimeFromHours
  ) where

import Prelude

import Data.Char (fromCharCode)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant, toDateTime)
import Data.Enum (toEnum)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Time (Time(Time))
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Partial.Unsafe (unsafePartial)

unsafeCharFromCharCode ∷ Int → Char
unsafeCharFromCharCode i =
  unsafePartial $ fromJust $ fromCharCode i

unsafeNonEmptyString ∷ String → NonEmptyString
unsafeNonEmptyString s =
  unsafePartial $ fromJust $ NES.fromString s

unsafeInstantFromSeconds ∷ Int → Instant
unsafeInstantFromSeconds seconds =
  unsafePartial $ fromJust $ instant $ convertDuration $ Seconds $
    toNumber seconds

unsafeDateTimeFromSeconds ∷ Int → DateTime
unsafeDateTimeFromSeconds = toDateTime <<< unsafeInstantFromSeconds

unsafeTimeFromHours ∷ Int → Time
unsafeTimeFromHours hour = unsafePartial $ fromJust $ do
  h ← toEnum hour
  m ← toEnum 0
  s ← toEnum 0
  millis ← toEnum 0
  pure $ Time h m s millis

unsafeDate ∷ { day ∷ Int, month ∷ Int, year ∷ Int } → Date
unsafeDate { day, month, year } = unsafePartial $ fromJust $ do
  y ← toEnum year
  m ← toEnum month
  d ← toEnum day
  Date.exactDate y m d
