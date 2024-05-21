module Redis.Script (Script (..), script, evalString, mapKeys, keysTouchedByScript, paramNames, paramValues, parser, ScriptExpression (..)) where

import Data.Attoparsec.Text (Parser, char,choice, inClass, many1', skipSpace, takeWhile1, (<?>), endOfInput)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
-- import Control.Applicative ((<|>))
import qualified Set
import Prelude (pure)
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
  let _expr = Attoparsec.parseOnly parser str
  -- let parsedScript = case expr str of
  --       Left err -> Prelude.error <| "Failed to parse script: " ++ err
  --       Right parsed ->
  -- [|parsedScript|]
  Debug.todo "qqScript"

data ScriptExpression
  = ScriptText Text
  | ScriptVariable Text
  deriving (Show, Eq)

parser :: Parser (List ScriptExpression)
parser = do
  result <- many1' (choice [parseText, parseVariable]) <?> "Expected at least one"
  endOfInput
  pure <| result

parseText :: Parser ScriptExpression
parseText = do
  text <- takeWhile1 ('$' /=) <?> "Expected text"
  pure <| ScriptText text

parseVariable :: Parser ScriptExpression
parseVariable = do
  _ <- char '$' <?> "Expected '$'"
  _ <- char '{' <?> "Expected '{'"
  skipSpace <?> "Expected space after '{'"
  name <- (takeWhile1 (not << inClass "${}"))
    <?> "No '$', '{' or '}' allowed in interpolated expression. Note: I'm a simple parser and I don't support records inside ${}."
  _ <- char '}' <?> "Expected '}' after: ${" ++ Text.toList name
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
