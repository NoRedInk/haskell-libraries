module Postgres.TH where

import qualified Environment
import qualified Language.Haskell.TH as TH
import Database.PostgreSQL.Typed.TH (useTPGDatabase)
import qualified Postgres.Settings
import qualified Prelude

-- | Template Haskell to connect to the database using NRI settings at compile time.  
useNRIDatabase :: TH.Q [TH.Dec]
useNRIDatabase = do
  pgDatabase <- TH.runIO (Prelude.fmap Postgres.Settings.toPGDatabase (Environment.decode Postgres.Settings.decoder))
  useTPGDatabase pgDatabase