{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Internal.Http where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import Data.Typeable
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Mime as Mime
import qualified Platform
import Prelude (IO)

-- |
data Handler
  = Handler
      { handlerRequest :: forall expect. Settings expect -> Task Error expect,
        handlerWithThirdParty :: forall a e. (HTTP.Manager -> Task e a) -> Task e a,
        handlerWithThirdPartyIO :: forall a. Platform.LogHandler -> (HTTP.Manager -> IO a) -> IO a
      }

-- |
data Settings a
  = Settings
      { _method :: Text,
        _headers :: [Header.Header],
        _url :: Text,
        _body :: Body,
        _timeout :: Maybe Int,
        _expect :: Expect a
      }

-- |  Represents the body of a Request.
data Body
  = Body
      { bodyContents :: Data.ByteString.Lazy.ByteString,
        bodyContentType :: Maybe Mime.MimeType
      }

-- |
-- Logic for interpreting a response body.
data Expect a where
  ExpectJson :: (Typeable a, Aeson.FromJSON a) => Expect a
  ExpectText :: Expect Text
  ExpectWhatever :: Expect ()

-- |
data Error
  = BadUrl Text
  | BadStatus Int
  | BadBody Text
  | BadResponse Text
  | Timeout
  | NetworkError Text
  deriving (Eq, Show)

instance Exception.Exception Error
