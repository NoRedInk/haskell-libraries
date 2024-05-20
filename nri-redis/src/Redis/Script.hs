{-# LANGUAGE TemplateHaskell #-}

module Redis.Script (Script(..), script, evalString, mapKeys, keysTouchedByScript, paramNames, paramValues) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import Data.ByteString (ByteString)
import qualified Set
import Data.Text.Encoding (encodeUtf8)
import qualified Prelude
import qualified Database.Redis

data Script result = Script
  { -- | The Lua script to be executed
    luaScript :: Text,
    -- | The parameters that fill the placeholders in this query
    params :: Log.Secret [Param],
    -- | The script string as extracted from a `script` quasi quote.
    quasiQuotedString :: Text
  } deriving (Eq, Show)

data Param = Param
  { kind :: ParamKind,
    name :: Text,
    value :: Text
  }
  deriving (Eq, Show)

data ParamKind = RedisKey | ArbitraryValue
  deriving (Eq, Show)

script :: QQ.QuasiQuoter
script =
  QQ.QuasiQuoter
    { QQ.quoteExp = qqScript,
      QQ.quoteType = Prelude.error "script not supported in types",
      QQ.quotePat = Prelude.error "script not supported in patterns",
      QQ.quoteDec = Prelude.error "script not supported in declarations"
    }

qqScript :: Prelude.String -> TH.ExpQ
qqScript scriptWithVars = do
  -- let bs = encodeUtf8 (Text.fromList scriptWithVars)
  let str = Text.fromList scriptWithVars
  [|Script str [] str|]

-- | EVAL script numkeys [key [key ...]] [arg [arg ...]]
evalString :: Script a -> Text
evalString = Debug.todo "evalString"

-- | Map the keys in the script to the keys in the Redis API
mapKeys :: (Text -> Task err Text) -> Script a -> Task err (Script a)
mapKeys _fn _script = Debug.todo "mapKeys"

-- | Get the keys touched by the script
keysTouchedByScript :: Script a -> Set.Set Text
keysTouchedByScript = Debug.todo "keysTouchedByScript"

-- | Get the parameter names in the script
paramNames :: Script a -> List Text
paramNames script' =
  script'
    |> params
    |> Log.unSecret
    |> List.map (\param -> name param)

-- | Get the parameter values in the script
paramValues :: Script a -> List Text
paramValues script' =
  script'
    |> params
    |> Log.unSecret
    |> List.map (\param -> value param)
