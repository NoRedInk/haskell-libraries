{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Data.Aeson
import Data.Proxy
import Examples
import qualified Redis
import Servant
import qualified Test
import qualified Test.Encoding.Redis
import qualified Test.Encoding.Routes
import qualified Prelude

main :: Prelude.IO ()
main = Test.run tests

data Foo = Foo
  { id :: Int,
    name :: Maybe Text
  }
  deriving (Generic)

instance FromJSON Foo

instance ToJSON Foo

instance HasExamples Foo where
  examples _ =
    example
      "Foo"
      [ Foo
          { id = 1,
            name = Just "foo"
          },
        Foo
          { id = 1,
            name = Nothing
          }
      ]

data Bar = Bar
  { title :: Text,
    description :: Text
  }
  deriving (Generic)

instance ToJSON Bar

instance HasExamples Bar where
  examples _ =
    example
      "Bar"
      Bar
        { title = "title",
          description = "description"
        }

data Baz = Baz
  { title :: Text,
    description :: Text
  }
  deriving (Generic)

instance ToJSON Baz

instance HasExamples Baz where
  examples _ =
    example
      "Baz"
      Baz
        { title = "title",
          description = "description"
        }

data Qux = Qux
  { title :: Text,
    description :: Text
  }
  deriving (Generic)

instance ToJSON Qux

instance HasExamples Qux where
  examples _ =
    example
      "Qux"
      Qux
        { title = "title",
          description = "description"
        }

data Routes route = Routes
  { foo ::
      route
        :- "foos"
          :> Capture "id" Int
          :> Get '[JSON] Foo,
    bars ::
      route
        :- "bars"
          :> Get '[JSON] [Bar],
    baz :: route :- NamedRoutes BazRoutes
  }
  deriving (Generic)

data BazRoutes route = BazRoutes
  { baz ::
      route
        :- "baz"
          :> Get '[JSON] Baz,
    qux :: route :- "quxs" :> Capture "id" Int :> NamedRoutes QuxRoutes
  }
  deriving (Generic)

newtype QuxRoutes route = QuxRoutes
  { qux ::
      route
        :- "details"
          :> Get '[JSON] Qux
  }
  deriving (Generic)

fooStore :: Redis.Api Text Foo
fooStore = Redis.jsonApi identity

tests :: Test.Test
tests =
  Test.describe
    "Test.Encoding"
    [ Test.describe
        "Routes.tests"
        (Test.Encoding.Routes.tests (Proxy :: Proxy Routes)),
      Test.describe
        "Redis.tests"
        [Test.Encoding.Redis.test fooStore]
    ]
