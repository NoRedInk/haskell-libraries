{-# OPTIONS_GHC -fno-warn-orphans #-}

module Redis.Codec where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Debug
import qualified Flat
import NriPrelude
import qualified Redis.Internal as Internal
import qualified Prelude

data Codec a = Codec
  { codecEncoder :: Encoder a,
    codecDecoder :: Decoder a
  }

type Encoder a = a -> ByteString

type Decoder a = ByteString -> Result Internal.Error a

flatCodec :: Flat.Flat a => Codec a
flatCodec =
  Codec Flat.flat <| \x ->
    case Flat.unflat x of
      Prelude.Right a -> Ok a
      Prelude.Left err ->
        Debug.toString err
          |> Internal.DecodingError
          |> Err

jsonCodec :: (Aeson.FromJSON a, Aeson.ToJSON a) => Codec a
jsonCodec = Codec jsonEncoder jsonDecoder

jsonEncoder :: Aeson.ToJSON a => Encoder a
jsonEncoder = Aeson.encode >> Data.ByteString.Lazy.toStrict

jsonDecoder :: Aeson.FromJSON a => Decoder a
jsonDecoder byteString =
  case Aeson.eitherDecodeStrict' byteString of
    Prelude.Right decoded -> Ok decoded
    Prelude.Left err ->
      Internal.DecodingError (Data.Text.pack err)
        |> Err

byteStringCodec :: Codec ByteString
byteStringCodec = Codec identity Ok

textCodec :: Codec Text
textCodec = Codec Data.Text.Encoding.encodeUtf8 (Data.Text.Encoding.decodeUtf8 >> Ok)
