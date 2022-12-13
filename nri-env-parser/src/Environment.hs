{-# LANGUAGE DeriveFunctor #-}

-- | A module for reading configuration options from environment variables.
--
-- Applications have configuration options. [The Twelve-Factor
-- App](https://12factor.net/import) recommends applications read these from
-- environment variables. This requires us to decode environment variables,
-- which are strings, into the different types the app's configuration options
-- might have. This module helps with that.
--
-- Here's what sets this package apart from other environment parsers:
--
-- - Very small API, supporting just one way to do environment parsing.
-- - Comes with parsers for common configuration option types, such as URIs.
--   Not using type classes for these parsers means we don't have to write a
--   bunch of orphan instances.
-- - Mandatory documentation of each environment variable we want to decode.
-- - The decoders keep track of all the environment variables they depend on.
--   That way the decoder for an application can tell us all the environment
--   variables an application depends on and what they are used for.
module Environment
  ( -- * Parsers
    Parser,
    text,
    int,
    float,
    boolean,
    uri,
    filePath,
    networkURI,
    secret,
    oneOf,
    custom,

    -- * Decoders
    Decoder,
    consumes,
    Variable (Variable, name, description, defaultValue),
    variable,
    either,
    decode,
    decodeDefaults,
    decodePairs,

    -- * Decoders for just the variables
    decodeVariables,
    decodeVariablePairs,
    DecodedVariable
      ( DecodedVariable,
        decodedVariable,
        decodedCurrent,
        decodedErrors
      ),
  )
where

import qualified Data.Text
import qualified Debug
import qualified Dict
import qualified List
import qualified Log
import qualified Maybe
import qualified Network.URI
import NriPrelude
import qualified Result
import qualified System.Environment
import Text.Read (readMaybe)
import qualified Text.URI
import qualified Tuple
import Prelude
  ( Applicative,
    Either (Left, Right),
    FilePath,
    Functor,
    IO,
    Integer,
    Integral,
    Semigroup,
    fail,
    fromIntegral,
    mempty,
    pure,
  )

-- |
-- A function that can read values of a type from text. For example, a
-- @Parser Int@ knows how to read a string like "412" and extract from that the
-- number @412@.
--
-- Parsing functions can fail when they read a text that they do not understand.
-- For example, the @Parser Int@ parser will fail if ran against the string
-- "Not a number in the slightest!".
newtype Parser a
  = Parser (Text -> Result Text a)
  deriving (Functor)

-- | Parse a text from an environment variable.
text :: Parser Text
text = Parser Ok

-- | Parse an integer from an environment variable.
-- Works for any integer type (@Integer@, @Int@, @Int@, ...).
int :: (Integral a) => Parser a
int =
  Parser <| \str ->
    case readMaybe (Data.Text.unpack str) of
      Nothing -> Err ("Could not parse as integer: " ++ str)
      Just (n :: Integer) -> Ok (fromIntegral n)

-- | Parse a floating point number from an environment variable.
float :: Parser Float
float =
  Parser <| \str ->
    case readMaybe (Data.Text.unpack str) of
      Nothing -> Err ("Could not parse as float: " ++ str)
      Just n -> Ok n

-- | Parse a boolean from an environment variable.
boolean :: Parser Bool
boolean =
  Parser <| \str ->
    case readMaybe (Data.Text.unpack str) of
      Nothing -> Err ("Could not parse as boolean: " ++ str)
      Just x -> Ok x

-- | Parse a URI from an environment variable.
uri :: Parser Text.URI.URI
uri =
  Parser <| \str ->
    case Text.URI.mkURI str of
      Left err ->
        ["Unexpected exception parsing uri:", str, ". Error reads:", Debug.toString err]
          |> Data.Text.unwords
          |> Err
      Right x -> Ok x

-- | Parse a file path from an environment variable.
filePath :: Parser FilePath
filePath = Parser (Ok << Data.Text.unpack)

-- | Parse a secret value from an environment variable.
--
-- Check the documentation for the @Log@ module of @nri-prelude@ to learn more
-- about secrets.
secret :: Parser a -> Parser (Log.Secret a)
secret = map Log.mkSecret

-- | There's two @URI@ types that are in vogue in the Haskell ecosystem. We would
-- like to standardized on the @Text.URI@ package, since it's the more modern
-- @Text@ based version (no @Strings@ for us!), but most libraries require the
-- other type. This function helps convert.
networkURI :: Parser Network.URI.URI
networkURI =
  Parser <| \str ->
    case Network.URI.parseURI (Data.Text.unpack str) of
      Nothing -> Err "Oh no! We have a valid Network.URI.URI but can't seem to parse it as a Network.URI.URI."
      Just uri' -> Ok uri' {Network.URI.uriPath = ""}

-- | Create a parser for custom types.
--
-- > data Environment = Development | Production
-- >
-- > environment :: Parser Environment
-- > environment =
-- >     oneOf text [
-- >         ( "development", Development),
-- >         ( "production", Production)
-- >     ]
oneOf :: (Show a, Ord a) => Parser a -> List (a, b) -> Parser b
oneOf toValue options =
  custom toValue <| \str ->
    case Dict.fromList options
      |> Dict.get str of
      Nothing ->
        [ "Unknown option:",
          Debug.toString str,
          "(",
          options
            |> List.map (Tuple.first >> Debug.toString)
            |> Text.join ", ",
          ")"
        ]
          |> Text.join " "
          |> Err
      Just x -> Ok x

-- | Create a parser for custom types. Build on the back of one of the primitve
-- parsers from this module.
--
-- > data Environment = Development | Production
-- >
-- > environment :: Parser Environment
-- > environment =
-- >     custom text <| \str ->
-- >         case str of
-- >             "development" -> Ok Development
-- >             "production" -> Ok Production
-- >             _ -> Err ("Unknown environment: " ++ str)
custom :: Parser a -> (a -> Result Text b) -> Parser b
custom (Parser base) fn = Parser (\val -> base val |> andThen fn)

-- | An environment decoder knows how to read an app's configuration from
-- environment variables. Check out the @variable@ function to see how you can
-- begin building decoders.
data Decoder config = Decoder
  { -- | The list of @Variable@s that this decoder will read when ran.
    consumes :: [Variable],
    readFromEnvironment :: Dict.Dict Text Text -> Result [ParseError] config
  }
  deriving (Functor)

instance Applicative Decoder where
  pure x = Decoder [] (\_ -> Ok x)

  (Decoder consumes1 f) <*> (Decoder consumes2 x) =
    Decoder
      { consumes = consumes1 ++ consumes2,
        readFromEnvironment = readFromEnvironment' f x
      }
    where
      readFromEnvironment' ::
        Semigroup err =>
        (env -> Result err (a -> config)) ->
        (env -> Result err a) ->
        env ->
        Result err config
      readFromEnvironment' f' x' env =
        -- This is the same as <*> except that when both sides are errors we
        -- want to capture both, not just the first.
        case (f' env, x' env) of
          (Err fe, Err xe) ->
            Err (fe ++ xe)
          (fr, xr) ->
            fr <*> xr

-- | An environment variable with a description of what it is used for.
data Variable = Variable
  { name :: Text,
    description :: Text,
    defaultValue :: Text
  }
  deriving (Show)

data ParseError = ParseError
  { failingVariable :: Variable,
    failingReason :: Text
  }
  deriving (Show)

-- | Describe a decoded variable for informational purposes.
data DecodedVariable = DecodedVariable
  { decodedVariable :: Variable,
    decodedCurrent :: Maybe Text,
    -- A single environment variable can be decoded by multiple decoders,
    -- each with their own constraints.
    decodedErrors :: List Text
  }
  deriving (Show)

-- | Produce a configuration from a single environment veriable. Usually you
-- will combine these with @mapN@ functions to build larger configurations.
--
-- > Data Settings = Settings
-- >    { amountOfHats :: Int
-- >    , furLined :: Bool
-- >    }
-- >
-- > map2
-- >  Settings
-- >  (variable (Variable "HATS" "Amount of hats" "2") int)
-- >  (variable (Variable "FUR_LINED" "Do hats have fur lining?" "False") boolean)
variable :: Variable -> Parser a -> Decoder a
variable var (Parser parse) =
  Decoder
    { consumes = [var],
      readFromEnvironment = \env ->
        let value =
              Dict.get (name var) env
                |> Maybe.withDefault (defaultValue var)
         in parse value |> Result.mapError (pure << ParseError var)
    }

-- | If the first decoder fails, try the second.
either :: Decoder a -> Decoder a -> Decoder a
either (Decoder consumes1 fa) (Decoder consumes2 fb) =
  Decoder
    { consumes = consumes1 ++ consumes2,
      readFromEnvironment = \env ->
        case fa env of
          Err ea -> case fb env of
            Err eb -> Err (ea ++ eb)
            Ok r -> Ok r
          Ok r -> Ok r
    }

-- | Attempt to decode a configuration by reading environment variables.
-- This will fail if one or more environment variables fail to parse.
--
-- It will not fail if certain environment variables are absent. Defaults will
-- be used for those missing values.
decode :: Decoder a -> IO a
decode configuration = do
  env <- getEnv
  case decodePairs configuration env of
    Err err -> fail (Data.Text.unpack err)
    Ok x -> pure x

-- | Same as 'decode', but takes the environment to decode as a dictionary.
decodePairs :: Decoder a -> Dict.Dict Text Text -> Result Text a
decodePairs configuration env =
  case readFromEnvironment configuration env of
    Err err -> Err (errorsToText err)
    Ok x -> Ok x

-- | Run a decoder. Instead of returnin the decoded value return metadata about
-- each variable that was decoded.
--
-- This can be helpful when generating a @--help@ command, for listing all the
-- variables that the application supports and what they are currently set to.
decodeVariables :: Decoder a -> IO [DecodedVariable]
decodeVariables configuration =
  fmap (decodeVariablePairs configuration) getEnv

-- | Same as 'decodeVariables', but takes the environment to decode as a
-- dictionary.
decodeVariablePairs :: Decoder a -> Dict.Dict Text Text -> [DecodedVariable]
decodeVariablePairs configuration env = do
  var <- consumes configuration
  pure
    ( DecodedVariable
        { decodedVariable = var,
          decodedCurrent = Dict.get (name var) env,
          decodedErrors =
            Dict.get (name var) errors
              |> Maybe.withDefault []
        }
    )
  where
    errors =
      map errorPair parseErrors
        |> List.foldl insert Dict.empty
    insert (k, v) dict =
      Dict.update
        k
        ( \prev ->
            case prev of
              Nothing -> Just v
              Just v' -> Just (v' ++ v)
        )
        dict
    errorPair err =
      ((failingVariable >> name) err, [failingReason err])
    parseErrors =
      case readFromEnvironment configuration env of
        Err errs -> errs
        Ok _ -> []

getEnv :: IO (Dict.Dict Text Text)
getEnv = do
  pairs <- System.Environment.getEnvironment
  pure <| Dict.fromList <| map (Tuple.mapBoth Data.Text.pack Data.Text.pack) pairs

-- | Build a configuration using only default values of environment variables.
-- Similar to @decode@, except this version doesn't read any environment
-- variables.
--
-- This is sometimes useful for tests, where you might not care about the exact
-- values of settings.
decodeDefaults :: Decoder a -> Result Text a
decodeDefaults configuration =
  readFromEnvironment configuration mempty |> Result.mapError errorsToText

errorsToText :: [ParseError] -> Text
errorsToText errors = map errorToText errors |> Data.Text.intercalate "\n\n"

errorToText :: ParseError -> Text
errorToText ParseError {failingVariable, failingReason} =
  Data.Text.unwords ["Parsing", name failingVariable, "failed:", failingReason]
