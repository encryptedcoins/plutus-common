{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusAppsExtra.Api.Kupo where

import           Control.Exception             (SomeException)
import           Control.FromSum               (eitherToMaybe)
import           Control.Monad.Catch           (try)
import           Data.Data                     (Proxy (..))
import           Data.Default                  (Default (def))
import           GHC.Base                      (coerce)
import           GHC.TypeLits                  (AppendSymbol, KnownSymbol)
import           Ledger                        (Datum, DatumHash, Script, ScriptHash, Slot, TxId, Validator, ValidatorHash,
                                                Versioned)
import           Network.HTTP.Client           (HttpExceptionContent, Request)
import           Plutus.V2.Ledger.Api          (CurrencySymbol, TokenName)
import qualified Plutus.V2.Ledger.Api          as P
import           PlutusAppsExtra.Types.Error   (ConnectionError)
import           PlutusAppsExtra.Utils.Kupo    (Kupo (..), KupoOrder, KupoResponse (..), Pattern (..), GetHealthResponse)
import           PlutusAppsExtra.Utils.Servant (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)
import           Servant.API                   (Capture, Get, JSON, QueryFlag, QueryParam, (:>))
import           Servant.Client                (client)

----------------------------------------------------------- Get Matches (*) -----------------------------------------------------------

type GetKupoResponse spentOrUnspent createdOrSpentBefore createdOrSpentAfter
    =             "matches"
    :> Capture    "pattern"            Pattern
    :> QueryFlag  spentOrUnspent
    :> QueryParam "order"              KupoOrder
    :> QueryParam createdOrSpentBefore (Kupo Slot)
    :> QueryParam createdOrSpentAfter  (Kupo Slot)
    :> QueryParam "policy_id"          (Kupo CurrencySymbol)
    :> QueryParam "asset_name"         (Kupo TokenName)
    :> QueryParam "transaction_id"     (Kupo TxId)
    :> QueryParam "output_index"       Integer
    :> Get '[JSON] [KupoResponse]

data SpentOrUnspent = SUSpent | SUUnspent
type family SpentOrUnspentSymbol su where
    SpentOrUnspentSymbol 'SUSpent   = "spent"
    SpentOrUnspentSymbol 'SUUnspent = "unspent"

data CreatedOrSpent = CSCreated | CSSpent
type family CreatedOrSpentSymbol cs where
    CreatedOrSpentSymbol 'CSCreated = "created"
    CreatedOrSpentSymbol 'CSSpent   = "spent"

data KupoRequest (su :: SpentOrUnspent) (before :: CreatedOrSpent) (after :: CreatedOrSpent)
    = KupoRequest
    { reqPattern              :: Pattern
    , reqSpentOrUnspent       :: Bool
    , reqOrder                :: Maybe KupoOrder
    , reqCreatedOrSpentBefore :: Maybe Slot
    , reqCreatedOrSpentAfter  :: Maybe Slot
    , reqCurrencySymbol       :: Maybe CurrencySymbol
    , reqTokenName            :: Maybe TokenName
    , reqTxId                 :: Maybe TxId
    , reqTxIdx                :: Maybe Integer
    } deriving (Show, Eq)

instance Default (KupoRequest su before after) where
    def = KupoRequest WildCardPattern False Nothing Nothing Nothing Nothing Nothing Nothing Nothing

type IsValidRequest su before after =
    ( KnownSymbol (SpentOrUnspentSymbol su)
    , KnownSymbol (AppendSymbol (CreatedOrSpentSymbol before) "_before")
    , KnownSymbol (AppendSymbol (CreatedOrSpentSymbol after) "_after")
    )

getKupoResponse :: forall su before after. IsValidRequest su before after => KupoRequest su before after -> IO [KupoResponse]
getKupoResponse KupoRequest{..} = getFromEndpointKupo $ client p
        reqPattern
        reqSpentOrUnspent
        reqOrder
        (Kupo <$> reqCreatedOrSpentBefore)
        (Kupo <$> reqCreatedOrSpentAfter)
        (Kupo <$> reqCurrencySymbol)
        (Kupo <$> reqTokenName)
        (Kupo <$> reqTxId)
        reqTxIdx
    where
        p = Proxy @(GetKupoResponse
            (SpentOrUnspentSymbol su)
            (AppendSymbol (CreatedOrSpentSymbol before) "_before")
            (AppendSymbol (CreatedOrSpentSymbol after)  "_after"))

------------------------------------------------------------- Get By hash -------------------------------------------------------------

type GetScriptByHash
    = "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Maybe (Kupo (Versioned Script)))

getScriptByHash :: ScriptHash -> IO (Maybe (Versioned Script))
getScriptByHash = fmap coerce . getFromEndpointKupo . client (Proxy @GetScriptByHash) . Kupo

type GetValidatorByHash
    = "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Maybe (Kupo (Versioned Validator)))

getValidatorByHash :: ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash = fmap coerce . getFromEndpointKupo . client (Proxy @GetValidatorByHash) . Kupo

type GetDatumByHash
    = "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getDatumByHash :: DatumHash -> IO (Maybe Datum)
getDatumByHash = fmap eitherToMaybe . try @IO @SomeException . fmap coerce . getFromEndpointKupo . client (Proxy @GetDatumByHash) . Kupo

------------------------------------------------------------ Get health -------------------------------------------------------------

type GetHealth = "health" :> Get '[JSON] GetHealthResponse

getHealth :: IO GetHealthResponse
getHealth = getFromEndpointKupo $ client (Proxy @GetHealth)

------------------------------------------------------------- Kupo port -------------------------------------------------------------

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content