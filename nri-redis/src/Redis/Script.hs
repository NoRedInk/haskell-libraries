{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Script
  ( Script (..),
    script,
    -- Internal API
    luaScriptHash,
    evalShaString,
    scriptLoadString,
    mapKeys,
    -- For testing
    parser,
    Tokens (..),
    ScriptParam (..),
    printScript,
  )
where

import qualified Control.Monad
import qualified Crypto.Hash.SHA1
import qualified Data.ByteString
import Data.Either (Either (..))
import qualified Data.Text
import qualified Data.Text.Encoding
import Data.Void (Void)
import qualified GHC.TypeLits
import Language.Haskell.Meta.Parse (parseExp)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as QQ
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Printf
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
  GHC.TypeLits.TypeError ('GHC.TypeLits.Text "[script| ${..} ] interpolation only supports Key or Literal inputs.") =>
  HasScriptParam x
  where
  getScriptParam = Prelude.error "This won't ever hit bc this generates a compile-time error."

-- | Quasi-quoter for creating a Redis Lua script with placeholders for Redis keys and arguments.
--
-- > [script|SET ${Key "a-redis-key"} ${Literal 123}|]
--
-- **IMPORTANT**: It is NOT SAFE to return Redis keys using this. Our Redis APIs inject
-- "namespaces" (prefixes) on keys, and any keys returned by Lua will have their namespaces
-- applied. If you try to reuse those keys in follow-up queries, namespaces will be doubly-applied.
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

----------------------------
-- Script template compile-time evaluation
----------------------------

data EvaluatedToken
  = EvaluatedText Text
  | EvaluatedVariable EvaluatedParam
  deriving (Show, Eq)

data EvaluatedParam = EvaluatedParam
  { kind :: ParamKind,
    value :: Text
  }
  deriving (Eq, Show)

data ParamKind = RedisKey | ArbitraryValue
  deriving (Eq, Show)

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

-----------------------------
-- Script record construction
-----------------------------

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

-----------------------------
-- Quasi-quoted text parser
-----------------------------

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

---------------------------------------------
-- Helper functions for internal library use
---------------------------------------------

-- | EVALSHA hash numkeys [key [key ...]] [arg [arg ...]]
evalShaString :: Script a -> Text
evalShaString script'@(Script {keys, arguments}) =
  let keyCount = keys |> List.length |> Text.fromInt
      keys' = keys |> Text.join " "
      args' = arguments |> Log.unSecret |> List.map (\_ -> "***") |> Text.join " "
      hash = luaScriptHash script'
   in "EVALSHA " ++ hash ++ " " ++ keyCount ++ " " ++ keys' ++ " " ++ args'

-- | SCRIPT LOAD "return KEYS[1]"
scriptLoadString :: Script a -> Text
scriptLoadString Script {luaScript} =
  "SCRIPT LOAD \"" ++ luaScript ++ "\""

-- | Map the keys in the script to the keys in the Redis API
mapKeys :: (Text -> Task err Text) -> Script a -> Task err (Script a)
mapKeys fn script' = do
  keys script'
    |> List.map fn
    |> Task.sequence
    |> Task.map (\keys' -> script' {keys = keys'})

luaScriptHash :: Script a -> Text
luaScriptHash Script {luaScript} =
  luaScript
    |> Data.Text.Encoding.encodeUtf8
    |> Crypto.Hash.SHA1.hash
    |> toHex

toHex :: Data.ByteString.ByteString -> Text
toHex bytes =
  bytes
    |> Data.ByteString.unpack
    |> List.map (Text.Printf.printf "%02x")
    |> List.concat
    |> Text.fromList

---------------------------------------------
-- Helper functions for testing
---------------------------------------------

printScript :: Script a -> Text
printScript Script {luaScript, quasiQuotedString, keys, arguments} =
  let listStr l = List.map (\s -> "\"" ++ s ++ "\"") l |> Text.join ", "
   in "Script { luaScript = \"" ++ luaScript ++ "\", quasiQuotedString = \"" ++ quasiQuotedString ++ "\", keys = [" ++ listStr keys ++ "], arguments = [" ++ listStr (Log.unSecret arguments) ++ "] }"
