{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module PlutusAppsExtra.Utils.Servant where

import           Control.Arrow               ((&&&))
import           Control.Monad.Catch         (Exception (..), MonadCatch, handle, throwM)
import           Control.Monad.IO.Class      (MonadIO (..))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Network.HTTP.Client         (HttpException (..), HttpExceptionContent (..), ManagerSettings (..), Request (port),
                                              defaultManagerSettings, newManager, responseTimeoutMicro)
import qualified Network.HTTP.Media          as M
import           Network.HTTP.Types.Status   (Status (statusCode))
import           PlutusAppsExtra.Types.Error (ConnectionError (..))
import           Servant.API                 (Accept (..), MimeUnrender (..))
import           Servant.API.ContentTypes    (MimeRender (..))
import           Servant.Client              (BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import qualified Servant.Client              as Servant

type Endpoint a = forall m. MonadIO m => ClientM a -> m a

getFromEndpointOnPort :: Int -> Endpoint a
getFromEndpointOnPort p endpoint = liftIO $ do
    manager <- newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro 60_000_000}
    responseOrError <- runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http "localhost" p ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response

pattern ConnectionErrorOnPort :: Int -> Request -> HttpExceptionContent -> ConnectionError
pattern ConnectionErrorOnPort port req c <- ConnectionError (id &&& port -> (req, port)) c

handle404 :: MonadCatch m => m a -> m a -> m a
handle404 h = handle $ \case
    Servant.FailureResponse _ ((statusCode . Servant.responseStatusCode -> 404)) -> h
    e                                                                            -> throwM e

handle404Maybe :: MonadCatch m => m a -> m (Maybe a)
handle404Maybe = handle404 (pure Nothing) . fmap Just

data CBOR

instance Accept CBOR where
    contentType _ = "application" M.// "cbor"

instance MimeRender CBOR BS.ByteString where
    mimeRender _ cbor = LBS.fromStrict cbor

instance MimeUnrender CBOR BS.ByteString where
    mimeUnrender _ lbs = pure $ LBS.toStrict lbs
