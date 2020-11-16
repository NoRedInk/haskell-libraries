-- | A module that automatically converts types to JSON prior to storing them in
-- Redis, and parses them back from JSON when reading.
--
-- When data is found in Redis but cannot be decoded these functions will return
-- an error.
--
-- TODO expose only one module `module Redis (fromCordec, Codec, Api, jsonCodec, textCodec) where`
module Redis.Generic
  ( -- * Creating api access functions
    fromCodec,
    Codec (..),
    Api (..),

    -- * Running queries
    Internal.Query,
    Internal.query,
    Internal.transaction,
    Internal.Handler,
    Internal.Error,
    Internal.map,
  )
where

import qualified Data.ByteString
import NriPrelude
import qualified Redis.Internal as Internal

data Codec a
  = Codec
      { encode :: a -> Data.ByteString.ByteString,
        decode ::
          -- TODO Remove Maybes
          Maybe Data.ByteString.ByteString ->
          Result Internal.Error (Maybe a)
      }

data Api a
  = Api
      { get :: Text -> Internal.Query (Maybe a),
        set :: Text -> a -> Internal.Query ()
      }

fromCodec :: Codec a -> Api a
fromCodec Codec {encode, decode} =
  Api
    { get = Internal.WithResult decode << (\key -> Internal.Get key),
      set = \key value -> Internal.Set key (encode value)
    }
