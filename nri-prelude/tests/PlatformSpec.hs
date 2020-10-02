module PlatformSpec (tests) where

import Data.Aeson as Aeson
import qualified Expect
import NriPrelude
import qualified Platform
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Platform"
    [ test "can recover custom span details from TracingSpanDetails" <| \_ ->
        CustomTracingSpanDetails "Hi!"
          |> Platform.toTracingSpanDetails
          |> Platform.fromTracingSpanDetails
          |> Expect.equal (Just (CustomTracingSpanDetails "Hi!"))
    ]

newtype CustomTracingSpanDetails = CustomTracingSpanDetails Text
  deriving (Aeson.ToJSON, Show, Eq)

instance Platform.TracingSpanDetails CustomTracingSpanDetails
