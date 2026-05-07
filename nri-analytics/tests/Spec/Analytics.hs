module Spec.Analytics (tests) where

import qualified Analytics
import qualified Analytics.Internal as Internal
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text
import qualified Dict
import qualified Environment
import qualified Expect
import qualified Log
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP.TLS
import NriPrelude
import Test (Test, describe, test)
import qualified Prelude

tests :: Test
tests =
  describe
    "Analytics"
    [ test "silentHandler.sendEvent does nothing and returns ()" <| \_ -> do
        Expect.fromIO <| Analytics.sendEvent Analytics.silentHandler (Aeson.object []),
      test "buildRequest sets URL, method, content-type, bearer auth" <| \_ -> do
        let settings =
              Internal.Settings
                { Internal.eventsServiceUrl = "https://events.example.com",
                  Internal.timeoutMicros = 250000,
                  Internal.authToken = Log.mkSecret "secret-token"
                }
        req <- Expect.fromIO <| Internal.buildRequest settings (Aeson.object [])
        let headers = HTTP.requestHeaders req
        HTTP.method req
          |> Expect.equal "POST"
        Prelude.lookup "Content-Type" headers
          |> Expect.equal (Just "application/json")
        Prelude.lookup "Authorization" headers
          |> Expect.equal (Just "Bearer secret-token"),
      test "stampEnvelope adds event_id and event_timestamp keys" <| \_ -> do
        result <- Expect.fromIO <| Internal.stampEnvelope (Aeson.object [("foo", Aeson.String "bar")])
        case result of
          Aeson.Object km -> do
            case KeyMap.lookup "event_id" km of
              Just (Aeson.String _) -> Expect.pass
              _ -> Expect.fail "event_id missing or wrong shape"
            case KeyMap.lookup "event_timestamp" km of
              Just (Aeson.String _) -> Expect.pass
              _ -> Expect.fail "event_timestamp missing or wrong shape"
            Expect.equal (KeyMap.lookup "foo" km) (Just (Aeson.String "bar"))
          _ -> Expect.fail "expected Object",
      test "decoder loads settings from env vars" <| \_ -> do
        let env =
              Dict.fromList
                [ ("ANALYTICS_EVENTS_SERVICE_URL", "https://events.example.com"),
                  ("ANALYTICS_EVENTS_TIMEOUT_MICROS", "300000"),
                  ("ANALYTICS_EVENTS_AUTH_TOKEN", "the-token")
                ]
        case Environment.decodePairs Analytics.decoder env of
          Ok s -> do
            Internal.eventsServiceUrl s
              |> Expect.equal "https://events.example.com"
            Internal.timeoutMicros s
              |> Expect.equal 300000
            Log.unSecret (Internal.authToken s)
              |> Expect.equal "the-token"
          Err err -> Expect.fail (Data.Text.pack (Prelude.show err)),
      test "sendEventIO swallows exceptions when delivery fails" <| \_ -> do
        -- Construct a settings pointing at an unreachable URL; verify that
        -- sendEventIO catches the resulting exception and returns () so
        -- analytics failure cannot propagate into the surrounding request.
        manager <- Expect.fromIO HTTP.TLS.newTlsManager
        let settings =
              Internal.Settings
                { Internal.eventsServiceUrl = "http://127.0.0.1:1",
                  Internal.timeoutMicros = 50000,
                  Internal.authToken = Log.mkSecret ""
                }
        Expect.fromIO <| Internal.sendEventIO manager settings (Aeson.object [("k", Aeson.String "v")])
    ]
