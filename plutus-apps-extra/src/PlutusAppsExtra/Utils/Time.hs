module PlutusAppsExtra.Utils.Time where

import           Cardano.Node.Emulator (SlotConfig, posixTimeToEnclosingSlot, posixTimeToUTCTime, slotToEndPOSIXTime,
                                        utcTimeToPOSIXTime)
import           Control.Applicative   (Alternative)
import           Data.Aeson.Types      (FromJSON (..), Parser, Value)
import           Data.Foldable         (asum)
import qualified Data.Text             as T
import           Data.Time             (UTCTime, defaultTimeLocale, parseTimeM)
import           Ledger                (Slot)
import qualified Data.Aeson as J

parseSlotOrUtc :: Value -> Parser (Either UTCTime Slot)
parseSlotOrUtc val = case val of
    J.String txt -> Left <$> parseTime (T.unpack txt)
    J.Number _   -> Right . fromInteger <$> parseJSON val
    J.Object _   -> Right <$> parseJSON val
    _            -> fail "parseSlotOrUtc"

parseTime :: (Alternative m, MonadFail m) => String -> m UTCTime
parseTime s = asum $ (\format -> parseTimeM True defaultTimeLocale format s)
    <$> ["%Y-%m-%d", "%Y-%m-%d-%H", "%Y-%m-%d-%H:%M", "%Y-%m-%d-%H:%M:%s"]

utcToSlot :: SlotConfig -> UTCTime -> Slot
utcToSlot conf = posixTimeToEnclosingSlot conf . utcTimeToPOSIXTime

slotToUtc :: SlotConfig -> Slot -> UTCTime
slotToUtc conf = posixTimeToUTCTime . slotToEndPOSIXTime conf
