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
import qualified Dict

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
    headers :: Dict.Dict Text Text,
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
emptyDetails = Details Nothing Nothing Nothing Nothing Dict.empty Nothing Nothing Nothing

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON infoEncodingOptions
  toEncoding = Aeson.genericToEncoding infoEncodingOptions

infoEncodingOptions :: Aeson.Options
infoEncodingOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 ' ',
      Aeson.omitNothingFields = True
    }

-- | A wrapper around the 'Details' type to indicate that we're reporting an
-- incoming http, like a request sent from a frontend to an http server.
newtype Incoming = Incoming Details
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails Incoming

-- | A wrapper around the 'Details' type to indicate that this is an outgoing
-- http request, for example a request we're making to an external weather API.
newtype Outgoing = Outgoing Details
  deriving (Aeson.ToJSON)

instance Platform.TracingSpanDetails Outgoing
