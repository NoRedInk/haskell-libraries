module Redis.Script (Script (..), script, evalString, mapKeys, keysTouchedByScript, paramNames, paramValues, parser, Tokens (..)) where

import Data.Void (Void)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import qualified Set
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import Prelude (notElem, pure, (<*))
import qualified Prelude

data Script result = Script
  { -- | The Lua script to be executed
    luaScript :: Text,
    -- | The parameters that fill the placeholders in this query
    params :: Log.Secret [Param],
    -- | The script string as extracted from a `script` quasi quote.
    quasiQuotedString :: Text
  }
  deriving (Eq, Show)

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
  let str = Text.fromList scriptWithVars
  let _expr = P.parse parser "" str
  -- let parsedScript = case expr str of
  --       Left err -> Prelude.error <| "Failed to parse script: " ++ err
  --       Right parsed ->
  -- [|parsedScript|]
  Debug.todo "qqScript"

data Tokens
  = ScriptText Text
  | ScriptVariable Text
  deriving (Show, Eq)

type Parser = P.Parsec Void Text

parser :: Parser (List Tokens)
parser = do
  (P.some (parseText <|> parseVariable))
    <* P.eof

parseText :: Parser Tokens
parseText = do
  text <- P.takeWhile1P (Just "some plain text") (/= '$')
  pure <| ScriptText text

parseVariable :: Parser Tokens
parseVariable = do
  _ <- PC.string "${"
  _ <- PC.space
  name <- P.takeWhile1P (Just "anything but '$', '{' or '}' (no records, sorry)") (\t -> t `notElem` ['$', '{', '}'])
  _ <- PC.char '}'
  pure <| ScriptVariable <| Text.trim name

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
    |> List.map name

-- | Get the parameter values in the script
paramValues :: Script a -> List Text
paramValues script' =
  script'
    |> params
    |> Log.unSecret
    |> List.map value
