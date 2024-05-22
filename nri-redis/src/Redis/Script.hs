{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Redis.Script (Script (..), script, evalString, mapKeys, keysTouchedByScript, paramNames, paramValues, parser, Tokens (..), ScriptParam (..), printScript) where

import qualified Control.Monad
import Data.Either (Either (..))
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
    -- | The parameters that fill the placeholders in this query
    params :: Log.Secret [EvaluatedParam]
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

qqScript :: Prelude.String -> TH.Q TH.Exp
qqScript scriptWithVars = do
  let str = Text.fromList scriptWithVars
  let parseResult = P.parse parser "" str
  case parseResult of
    Left err -> Prelude.error <| "Failed to parse script: " ++ P.errorBundlePretty err
    Right tokens -> do
      let luaScript' = tokensToScript tokens
      let paramsExp =
            tokens
              |> List.filterMap
                ( \t ->
                    case t of
                      ScriptVariable name -> Just name
                      _ -> Nothing
                )
              -- Parse haskell syntax between ${} and convert to a TH Exp
              |> List.indexedMap (\idx exp -> varToExp exp |> scriptParamExpression (Prelude.fromIntegral idx))
              |> TH.ListE
              |> TH.AppE (TH.VarE 'Log.mkSecret)
      scriptConstructor <- [|Script luaScript' str|]
      pure <| scriptConstructor `TH.AppE` paramsExp

scriptParamExpression :: Prelude.Integer -> TH.Exp -> TH.Exp
scriptParamExpression idx exp =
  (TH.VarE 'evaluateScriptParam) `TH.AppE` TH.LitE (TH.IntegerL idx) `TH.AppE` exp

evaluateScriptParam :: HasScriptParam a => Int -> a -> EvaluatedParam
evaluateScriptParam idx scriptParam =
  case getScriptParam scriptParam of
    Key a ->
      EvaluatedParam
        { kind = RedisKey,
          name = "arg" ++ Text.fromInt idx,
          value = Debug.toString a
        }
    Literal a ->
      EvaluatedParam
        { kind = ArbitraryValue,
          name = "arg" ++ Text.fromInt idx,
          value = Debug.toString a
        }

varToExp :: Text -> TH.Exp
varToExp var =
  case parseExp (Text.toList var) of
    Left err -> Prelude.error <| "Failed to parse variable: " ++ err
    Right exp -> exp

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

tokensToScript :: List Tokens -> Text
tokensToScript tokens =
  tokens
    |> List.indexedMap
      ( \idx t ->
          case t of
            ScriptText text -> text
            ScriptVariable _ -> "@arg" ++ Text.fromInt idx
      )
    |> Text.join ""

-- | EVAL script numkeys [key [key ...]] [arg [arg ...]]
evalString :: Script a -> Text
evalString script' =
  let paramCount = script' |> params |> Log.unSecret |> List.length |> Text.fromInt
      paramKeys = paramNames script' |> Text.join " "
      paramArgs = paramValues script' |> List.map (\_ -> "***") |> Text.join " "
   in "EVAL " ++ luaScript script' ++ " " ++ paramCount ++ " " ++ paramKeys ++ " " ++ paramArgs

-- | Map the keys in the script to the keys in the Redis API
mapKeys :: (Text -> Task err Text) -> Script a -> Task err (Script a)
mapKeys fn script' = do
  newParams <-
    script'
      |> params
      |> Log.unSecret
      |> Control.Monad.mapM
        ( \param ->
            case kind param of
              RedisKey -> fn (value param) |> Task.map (\newValue -> param {value = newValue})
              ArbitraryValue -> pure param
        )
  pure <| script' {params = Log.mkSecret newParams}

-- | Get the keys touched by the script
keysTouchedByScript :: Script a -> Set.Set Text
keysTouchedByScript script' =
  script'
    |> params
    |> Log.unSecret
    |> List.filterMap
      ( \param ->
          case kind param of
            RedisKey -> Just (value param)
            ArbitraryValue -> Nothing
      )
    |> Set.fromList

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

printScript :: Script a -> Text
printScript Script {luaScript, quasiQuotedString, params} =
  let printableParams = Log.unSecret params
   in "Script { luaScript = \"" ++ luaScript ++ "\", quasiQuotedString = \"" ++ quasiQuotedString ++ "\", params = " ++ Debug.toString printableParams ++ " }"
