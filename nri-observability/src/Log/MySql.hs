module Log.MySql
  ( QuerySpan,
    emptyQuerySpan,
    query,
    queryTemplate,
    sqlOperation,
    queriedRelation,
    host,
    port,
    database,
    rowsReturned,
  )
where

import qualified Data.Aeson as Aeson

data QuerySpan = QuerySpan
  { -- | The full query we're executing.
    query :: Maybe (Log.Secret Text),
    -- | The query we're executing with values mocked out.
    queryTemplate :: Maybe Text,
    -- | The SQL operation we're performing (SELECT / INSERT / DELETE / ...).
    sqlOperation :: Maybe Text,
    -- | The primary relatino of the query.
    queriedRelation :: Maybe Text,
    -- | Database host the connection is made to.
    host :: Maybe Text,
    -- | Port the database is running on.
    port :: Maybe Int,
    -- | The name of the database that is being queried.
    database :: Maybe Text,
    -- | The amount of rows this query returned.
    rowsReturned :: Maybe Int
  }
  deriving (Generic)

emptyQuerySpan :: QuerySpan
emptyQuerySpan = QuerySpan Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Aeson.ToJSON QuerySpan where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 ' '}

instance Platform.TracingSpanDetails QuerySpan
