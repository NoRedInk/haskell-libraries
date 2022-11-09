{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Http.Internal where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Dynamic as Dynamic
import Dict (Dict)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Mime as Mime
import qualified Platform
import Prelude (IO)

-- | A handler for making HTTP requests.
data Handler = Handler
  { handlerRequest :: forall expect. (Dynamic.Typeable expect) => Request expect -> Task Error expect,
    handlerWithThirdParty :: forall e a. (HTTP.Manager -> Task e a) -> Task e a,
    handlerWithThirdPartyIO :: forall a. Platform.LogHandler -> (HTTP.Manager -> IO a) -> IO a
  }

-- | A custom request.
data Request a = Request
  { -- | The request method, like @"GET"@ or @"PUT"@.
    method :: Text,
    -- | A list of request headers.
    headers :: [Header],
    -- | The url, like @"https://fishes.com/salmon"@.
    url :: Text,
    -- | The request body.
    body :: Body,
    -- | The amount of microseconds you're willing to wait before giving up.
    timeout :: Maybe Int,
    -- | The type of response you expect back from the request.
    expect :: Expect a
  }

-- | An HTTP header for configuration requests.
newtype Header = Header {unHeader :: Header.Header}
  deriving (Eq, Show)

-- | Represents the body of a Request.
data Body = Body
  { bodyContents :: Data.ByteString.Lazy.ByteString,
    bodyContentType :: Maybe Mime.MimeType
  }

-- |
-- Logic for interpreting a response body.
data Expect a where
  ExpectJson :: Aeson.FromJSON a => Expect a
  ExpectText :: Expect Text
  ExpectWhatever :: Expect ()
  ExpectTextResponse :: (Response Text -> Result Error a) -> Expect a
  ExpectBytesResponse :: (Response Data.ByteString.ByteString -> Result Error a) -> Expect a

-- | A 'Request' can fail in a couple of ways:
--
-- - 'BadUrl' means you did not provide a valid URL.
-- - 'Timeout' means it took too long to get a response.
-- - 'NetworkError' means the user turned off their wifi, went in a cave, etc.
-- - 'BadStatus' means you got a response back, but the status code indicates failure.
-- - 'BadBody' means you got a response back with a nice status code, but the body of the response was something unexpected. The 'Text' in this cse is the debugging message that explains what went wrong with your JSONT decoder or whatever.
data Error
  = BadUrl Text
  | Timeout
  | NetworkError Text
  | BadStatus Int
  | BadBody Text
  deriving (Generic, Eq, Show)

instance Exception.Exception Error

instance Aeson.ToJSON Error

-- | A 'Response' can come back a couple different ways:
--
-- - 'BadUrl_' — you did not provide a valid URL.
-- - 'Timeout_' — it took too long to get a response.
-- - 'NetworkError_' — the user turned off their wifi, went in a cave, etc.
-- - 'BadStatus_' — a response arrived, but the status code indicates failure.
-- - 'GoodStatus_' — a response arrived with a nice status code!
-- - The type of the body depends on whether you use expectStringResponse or expectBytesResponse.
data Response body
  = BadUrl_ Text
  | Timeout_
  | NetworkError_ Text
  | BadStatus_ Metadata body
  | GoodStatus_ Metadata body
  deriving (Generic, Eq, Show)

instance (Dynamic.Typeable body, Show body) => Exception.Exception (Response body)

instance (Aeson.ToJSON body) => Aeson.ToJSON (Response body)

-- Extra information about the response:
--
-- Note: It is possible for a response to have the same header multiple times. In that case, all the values end up in a single entry in the headers dictionary. The values are separated by commas, following the rules outlined here.
data Metadata = Metadata
  { -- statusCode like 200 or 404
    metadataStatusCode :: Int,
    -- statusText describing what the statusCode means a little
    metadataStatusText :: Text,
    -- headers like Content-Length and Expires
    metadataHeaders :: Dict Text Text
  }
  deriving (Generic, Eq, Show)

instance Aeson.ToJSON Metadata
