{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Internal.Http where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Mime as Mime
import qualified Platform
import Prelude (IO)

-- | A handler for making HTTP requests.
data Handler = Handler
  { handlerRequest :: forall expect. Request expect -> Task Error expect,
    handlerWithThirdParty :: forall a e. (HTTP.Manager -> Task e a) -> Task e a,
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
