-- | A module for creating great logs in code that send or receive HTTP
-- requests.
module Log.HttpRequest
  ( Details,
    Incoming (Incoming),
    Outgoing (Outgoing),
    emptyDetails,
    method,
    host,
    path,
    queryString,
    headers,
    httpVersion,
    endpoint,
    status,
  )
where

import qualified Data.Aeson as Aeson

-- | A type describing an http request.
--
-- > emptyDetails
-- >   { method = Just "GET"
-- >   , host = Just "https://noredink.com"
-- >   }
data Details = Details
  { -- | The method of the http request.
    method :: Maybe Text,
    -- | The host the http request is made to.
    host :: Maybe Text,
    -- | The path portion of the request URI.
    path :: Maybe Text,
    -- | The query string portion of the request URI.
    queryString :: Maybe Text,
    -- | The headers on the request. Do not pass headers with sensitive
    -- information in there, filter them out first!
    headers :: [(Text, Text)],
    -- | The version of the http protocol used.
    httpVersion :: Maybe Text,
    -- | The endpoint called. This is like the path, but with the dynamic parts
    -- of the path replaced with arguments.
    --
    -- For example:
    -- > path     /teeth/upperleft/12
    -- > endpoint /teeth/:quadrant/:number
    endpoint :: Maybe Text,
    -- | The response status of the request.
    status :: Maybe Int
  }
  deriving (Generic)

-- | An empty details value to be modified by you.
emptyDetails :: Details
emptyDetails = Details Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON infoEncodingOptions
  toEncoding = Aeson.genericToEncoding infoEncodingOptions

infoEncodingOptions :: Aeson.Options
infoEncodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 ' '
    }

newtype Incoming = Incoming Details
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails Incoming

newtype Outgoing = Outgoing Details
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails Outgoing
