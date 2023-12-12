{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test helpers to ensure we don't change types or encodings of types
-- used in our request and response bodies by accident. `crawl` will
-- traverse our entire `Routes` API for request and response body types,
-- and fail to compile if we don't provide example values for each. We
-- can then run a golden result test for each, meaning we encode each
-- example value to JSON and check it matches a known-good encoding we
-- have comitted to the repo.
module Test.Encoding.Routes (tests, tests', IsApi (..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (Proxy))
import qualified Data.Semigroup
import qualified Data.Typeable as Typeable
import qualified Debug
import qualified Examples
import qualified Expect
import qualified GHC.Stack as Stack
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified List
import NriPrelude
import qualified Servant
import Servant.API
  ( Capture',
    Header',
    QueryFlag,
    QueryParam',
    QueryParams,
    Raw,
    ReqBody',
    Summary,
    Verb,
    (:<|>),
    (:>),
  )
import Servant.API.Generic (ToServantApi)
import qualified Servant.Auth.Server
import Test (Test, describe, test)
import qualified Test.Encoding
import qualified Text

-- | Creates tests for routes and custom types used in routes.
--
-- Example usage:
--   describe
--     "Spec.ApiEncoding"
--     (TestEncoding.tests (Proxy :: Proxy Routes.Routes))
tests :: forall routes. (IsApi (ToServantApi routes), Stack.HasCallStack) => Proxy routes -> List Test
tests proxy =
  -- We need to freeze the call stack before proceeding.
  -- If we don't, the callstack for these generated tests would point to haskell-libraries code
  -- and not client code.
  -- This didn't matter that much until we had the ability to specify `--files tests/BlaSpec.hs`
  -- If we don't fix this, we can't ever `--files tests/SomeApiSpec.hs`
  Stack.withFrozenCallStack frozenTests proxy "test/golden-results/route-types.json"

-- | Creates tests for routes and custom types used in routes.
-- The second argument is the path to a golden result file for the definied routes.
--
-- Example usage:
--   describe
--     "Spec.ApiEncoding"
--     (TestEncoding.tests (Proxy :: Proxy Routes.Routes) "test/golden-results/my-route-types.json")
tests' :: forall routes. (IsApi (ToServantApi routes), Stack.HasCallStack) => Proxy routes -> Text -> List Test
tests' proxy goldenRouteTypesFile =
  -- We need to freeze the call stack before proceeding.
  -- If we don't, the callstack for these generated tests would point to haskell-libraries code
  -- and not client code.
  -- This didn't matter that much until we had the ability to specify `--files tests/BlaSpec.hs`
  -- If we don't fix this, we can't ever `--files tests/SomeApiSpec.hs`
  Stack.withFrozenCallStack frozenTests proxy goldenRouteTypesFile

frozenTests :: forall routes. (IsApi (ToServantApi routes), Stack.HasCallStack) => Proxy routes -> Text -> List Test
frozenTests _ goldenRouteTypesFile =
  let routes = crawl (Proxy :: Proxy (ToServantApi routes))
   in [ test "route types haven't changed" <| \() ->
          routes
            |> routesToText
            |> Expect.equalToContentsOf goldenRouteTypesFile,
        describe
          "encodings of custom types"
          ( routes
              |> routesWithExamples
              |> List.map
                ( \(route, examples) ->
                    Test.Encoding.examplesToTest ("Examples for route `" ++ routeName route ++ "`") (routeToFileName route) examples
                )
          )
      ]

data Route = Route
  { path :: [Text],
    queryParams :: [(Text, SomeType)],
    method :: Text,
    headers :: [(Text, SomeType)],
    requestBody :: Maybe SomeType,
    responseBody :: SomeType
  }

data SomeType where
  SomeType :: (Typeable.Typeable t, Examples.HasExamples t) => Proxy t -> SomeType

routesWithExamples :: List Route -> List (Route, Examples.Examples)
routesWithExamples routes =
  routes
    |> List.map
      ( \route@Route {requestBody, responseBody, headers} ->
          ( route,
            case (requestBody, responseBody) of
              (Nothing, SomeType t) ->
                List.map headersToExamples headers
                  |> (NonEmpty.:|) (Examples.examples t)
                  |> Data.Semigroup.sconcat
              (Just (SomeType s), SomeType t) ->
                List.map headersToExamples headers
                  |> (NonEmpty.:|) (Examples.examples s ++ Examples.examples t)
                  |> Data.Semigroup.sconcat
          )
      )
  where
    headersToExamples (_, SomeType t) = Examples.examples t

routeName :: Route -> Text
routeName route =
  Text.join " " [method route, Text.join "/" (path route)]

routeToFileName :: Route -> Text
routeToFileName route =
  method route ++ "-" ++ Text.join "-" (path route) ++ ".json"

routesToText :: List Route -> Text
routesToText routes =
  routes
    |> List.concatMap
      ( \route ->
          [ case queryParams route of
              [] -> Nothing
              queryParams' ->
                Just
                  <| Text.concat
                    ( routeName route
                        : "?"
                        : [Text.join "&" (List.map printQueryParam queryParams')]
                    ),
            case headers route of
              [] -> Nothing
              headers' ->
                Just
                  <| Text.join
                    " "
                    ( routeName route
                        : "headers"
                        : List.map printHeaders headers'
                    ),
            Just
              <| Text.join
                " "
                [ routeName route,
                  "response",
                  printType (responseBody route)
                ],
            case requestBody route of
              Nothing -> Nothing
              Just body ->
                Just
                  <| Text.join
                    " "
                    [ routeName route,
                      "request",
                      printType body
                    ]
          ]
      )
    |> List.filterMap identity
    |> List.sort
    |> Text.join "\n"

printHeaders :: (Text, SomeType) -> Text
printHeaders (key, val) =
  "(" ++ key ++ ", " ++ printType val ++ ")"

printQueryParam :: (Text, SomeType) -> Text
printQueryParam (key, val) =
  key ++ "={" ++ printType val ++ "}"

printType :: SomeType -> Text
printType (SomeType t) =
  Typeable.typeRep t
    |> Debug.toString

-- | A helper type class that provides us example values of particular types.
-- The `IsApi` typeclass below will demand we define an instance of this type
-- class for each type used in a request or response body.

-- | A helper type class that can crawl our servant `Routes` type and return us
-- JSON-encoded examples for each request and response body type in that API.
-- Example usage:
--
--   routes = crawl (Proxy :: Proxy (ToServantApi Routes.Routes))
class IsApi a where
  crawl :: Proxy a -> [Route]

instance (IsApi a, IsApi b) => IsApi (a :<|> b) where
  crawl _ = crawl (Proxy :: Proxy a) ++ crawl (Proxy :: Proxy b)

instance (KnownSymbol s, IsApi a) => IsApi (s :> a) where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { path =
                  Text.fromList (symbolVal (Proxy :: Proxy s))
                    : path route
              }
        )

instance (KnownSymbol s, IsApi a) => IsApi (Capture' mods s paramType :> a) where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { path = (":" ++ Text.fromList (symbolVal (Proxy :: Proxy s))) : path route
              }
        )

instance
  (Typeable.Typeable method, Typeable.Typeable body, Examples.HasExamples body) =>
  IsApi (Verb method status encodings body)
  where
  crawl _ =
    [ Route
        { path = [],
          queryParams = [],
          headers = [],
          requestBody = Nothing,
          method =
            Typeable.typeRep (Proxy :: Proxy method)
              |> Debug.toString,
          responseBody = SomeType (Proxy :: Proxy body)
        }
    ]

instance (IsApi a) => IsApi (Servant.Auth.Server.Auth types user :> a) where
  crawl _ = crawl (Proxy :: Proxy a)

instance IsApi Raw where
  crawl _ = []

instance (IsApi a) => IsApi (QueryFlag flag :> a) where
  crawl _ = crawl (Proxy :: Proxy a)

instance
  ( KnownSymbol key,
    Typeable.Typeable value,
    Examples.HasExamples value,
    IsApi a
  ) =>
  IsApi (QueryParam' x key value :> a)
  where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { queryParams =
                  ( Text.fromList (symbolVal (Proxy :: Proxy key)),
                    SomeType (Proxy :: Proxy value)
                  )
                    : queryParams route
              }
        )

instance
  ( KnownSymbol key,
    Typeable.Typeable value,
    Examples.HasExamples value,
    IsApi a
  ) =>
  IsApi (QueryParams key value :> a)
  where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { queryParams =
                  ( Text.fromList (symbolVal (Proxy :: Proxy key)) ++ "[]",
                    SomeType (Proxy :: Proxy value)
                  )
                    : queryParams route
              }
        )

instance (Typeable.Typeable body, Examples.HasExamples body, IsApi a) => IsApi (ReqBody' x encodings body :> a) where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { requestBody = Just (SomeType (Proxy :: Proxy body))
              }
        )

instance
  ( KnownSymbol key,
    Typeable.Typeable val,
    Examples.HasExamples val,
    IsApi a
  ) =>
  IsApi (Header' mods key val :> a)
  where
  crawl _ =
    crawl (Proxy :: Proxy a)
      |> List.map
        ( \route ->
            route
              { headers =
                  ( Text.fromList (symbolVal (Proxy :: Proxy key)),
                    SomeType (Proxy :: Proxy val)
                  )
                    : headers route
              }
        )

instance (IsApi a) => IsApi (Summary x :> a) where
  crawl _ = crawl (Proxy :: Proxy a)

instance Examples.HasExamples Servant.NoContent where
  examples _ = Examples.example "NoContent" ()
