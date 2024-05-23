{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Script (Script (..), script, evalString, mapKeys, keysTouchedByScript, parser, Tokens (..), ScriptParam (..), printScript) where

import qualified Control.Monad
import Data.Either (Either (..))
import qualified Data.Text
import Data.Void (Void)
import qualified GHC.TypeLits
import Language.Haskell.Meta.Parse (parseExp)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import qualified Set
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import Prelude (notElem, pure, (<*))
import qualified Prelude

data Script result = Script
  { -- | The Lua script to be executed with @args placeholders for Redis
    luaScript :: Text,
    -- | The script string as extracted from a `script` quasi quote.
    quasiQuotedString :: Text,
    keys :: [Text],
    -- | The parameters that fill the placeholders in this query
    arguments :: Log.Secret [Text]
  }
  deriving (Eq, Show)

-- | A type for enforcing parameters used in [script|${ ... }|] are either tagged as Key or Literal.
--
-- We need keys to be tagged, otherwise we can't implement `mapKeys` and enforce namespacing
-- in Redis APIs.
--
-- We make this extra generic to allow us to provide nice error messages using TypeError in a
-- type class below.
data ScriptParam
  = forall a. (Show a) => Key a
  | forall a. (Show a) => Literal a

class HasScriptParam a where
  getScriptParam :: a -> ScriptParam

instance HasScriptParam ScriptParam where
  getScriptParam = Prelude.id

-- | This instance is used to provide a helpful error message when a user tries to use a type
-- other than a ScriptParam in a [script|${ ... }|] quasi quote.
--
-- It is what forces us to have IncoherentInstances and UndecidedInstances enabled.
instance
  {-# OVERLAPPABLE #-}
  GHC.TypeLits.TypeError (GHC.TypeLits.Text "[script| ${..} ] interpolation only supports Key or Literal inputs.") =>
  HasScriptParam x
  where
  getScriptParam = Prelude.error "This won't ever hit bc this generates a compile-time error."

data EvaluatedParam = EvaluatedParam
  { kind :: ParamKind,
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

qqScript :: Prelude.String -> TH.Q TH.Exp
qqScript scriptWithVars = do
  let quotedScript = Text.fromList scriptWithVars
  let parseResult = P.parse parser "" quotedScript
  case parseResult of
    Left err -> Prelude.error <| "Failed to parse script: " ++ P.errorBundlePretty err
    Right tokens -> do
      paramsExp <-
        tokens
          |> Control.Monad.mapM toEvaluatedToken
          |> map TH.ListE
      quotedScriptExp <- [|quotedScript|]
      pure <| (TH.VarE 'scriptFromEvaluatedTokens) `TH.AppE` quotedScriptExp `TH.AppE` paramsExp

data ScriptBuilder = ScriptBuilder
  { buffer :: Text,
    keyIdx :: Int,
    keyList :: List Text,
    argIdx :: Int,
    argList :: List Text
  }

scriptFromEvaluatedTokens :: Text -> [EvaluatedToken] -> Script a
scriptFromEvaluatedTokens quasiQuotedString' evaluatedTokens =
  let keyTpl n = "KEYS[" ++ Text.fromInt n ++ "]"
      argTpl n = "ARGV[" ++ Text.fromInt n ++ "]"
      script' =
        List.foldl
          ( \token scriptBuilder@(ScriptBuilder {buffer, keyIdx, keyList, argIdx, argList}) ->
              case token of
                EvaluatedText text -> scriptBuilder {buffer = buffer ++ text}
                EvaluatedVariable var ->
                  case kind var of
                    RedisKey ->
                      scriptBuilder
                        { buffer = buffer ++ keyTpl (keyIdx + 1),
                          keyIdx = keyIdx + 1,
                          keyList = value var : keyList
                        }
                    ArbitraryValue ->
                      scriptBuilder
                        { buffer = buffer ++ argTpl (argIdx + 1),
                          argIdx = argIdx + 1,
                          argList = value var : argList
                        }
          )
          (ScriptBuilder "" 0 [] 0 [])
          evaluatedTokens
   in Script
        { luaScript = buffer script',
          quasiQuotedString = quasiQuotedString',
          keys = keyList script',
          arguments = Log.mkSecret (argList script')
        }

toEvaluatedToken :: Tokens -> TH.Q TH.Exp
toEvaluatedToken token =
  case token of
    ScriptText text -> [|EvaluatedText text|]
    ScriptVariable var -> pure <| (TH.VarE 'evaluateScriptParam) `TH.AppE` (varToExp var)

evaluateScriptParam :: HasScriptParam a => a -> EvaluatedToken
evaluateScriptParam scriptParam =
  case getScriptParam scriptParam of
    Key a ->
      EvaluatedVariable
        <| EvaluatedParam
          { kind = RedisKey,
            value = unquoteString (Debug.toString a)
          }
    Literal a ->
      EvaluatedVariable
        <| EvaluatedParam
          { kind = ArbitraryValue,
            value = unquoteString (Debug.toString a)
          }

-- | Remove leading and trailing quotes from a string
unquoteString :: Text -> Text
unquoteString str =
  str
    |> Data.Text.stripPrefix "\""
    |> Maybe.andThen (Data.Text.stripSuffix "\"")
    |> Maybe.withDefault str

varToExp :: Text -> TH.Exp
varToExp var =
  case parseExp (Text.toList var) of
    Left err -> Prelude.error <| "Failed to parse variable: " ++ err
    Right exp -> exp

-- | Tokens after parsing quasi-quoted text
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

data EvaluatedToken
  = EvaluatedText Text
  | EvaluatedVariable EvaluatedParam
  deriving (Show, Eq)

-- | EVAL script numkeys [key [key ...]] [arg [arg ...]]
evalString :: Script a -> Text
evalString Script {luaScript, keys, arguments} =
  let keyCount = keys |> List.length |> Text.fromInt
      keys' = keys |> Text.join " "
      args' = arguments |> Log.unSecret |> List.map (\_ -> "***") |> Text.join " "
   in "EVAL {{" ++ luaScript ++ "}} " ++ keyCount ++ " " ++ keys' ++ " " ++ args'

-- | Map the keys in the script to the keys in the Redis API
mapKeys :: (Text -> Task err Text) -> Script a -> Task err (Script a)
mapKeys fn script' = do
  keys script'
    |> List.map fn
    |> Task.sequence
    |> Task.map (\keys' -> script' {keys = keys'})

-- | Get the keys touched by the script
keysTouchedByScript :: Script a -> Set.Set Text
keysTouchedByScript script' =
  keys script'
    |> Set.fromList

printScript :: Script a -> Text
printScript Script {luaScript, quasiQuotedString, keys, arguments} =
  let listStr l = List.map (\s -> "\"" ++ s ++ "\"") l |> Text.join ", "
   in "Script { luaScript = \"" ++ luaScript ++ "\", quasiQuotedString = \"" ++ quasiQuotedString ++ "\", keys = [" ++ listStr keys ++ "], arguments = [" ++ listStr (Log.unSecret arguments) ++ "] }"
