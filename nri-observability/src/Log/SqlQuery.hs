-- | A module for creating great logs in code that makes SQL queries.
module Log.SqlQuery
  ( Details,
    emptyDetails,
    query,
    queryTemplate,
    sqlOperation,
    queriedRelation,
    databaseType,
    host,
    port,
    database,
    rowsReturned,
    mysql,
    postgresql,
  )
where

import qualified Data.Aeson as Aeson

-- | A type describing an SQL query.
--
-- > emptyDetails
-- >   { query = Just (Log.mkSecret "SELECT cuddles FROM puppies")
-- >   , database = Just postgresql
-- >   }
data Details = Details
  { -- | The full query we're executing.
    query :: Maybe (Log.Secret Text),
    -- | The query we're executing with values mocked out.
    queryTemplate :: Maybe Text,
    -- | The SQL operation we're performing (SELECT / INSERT / DELETE / ...).
    sqlOperation :: Maybe Text,
    -- | The primary relation of the query.
    queriedRelation :: Maybe Text,
    -- | The type of database.
    databaseType :: Maybe Text,
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

-- | An empty details value to be modified by you.
emptyDetails :: Details
emptyDetails = Details Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Aeson.ToJSON Details where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = Aeson.camelTo2 ' '}

instance Platform.TracingSpanDetails Details

mysql :: Text
mysql = "MySQL"

postgresql :: Text
postgresql = "PostgreSQL"
