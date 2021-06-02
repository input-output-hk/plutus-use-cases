-- |

module Backend.Schema where

import Database.Beam.AutoMigrate
import Database.Beam.Postgres
import Database.Beam.Schema.Tables

-- import Backend.Orphans ()
import Common.Schema

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

dbAnn :: AnnotatedDatabaseSettings Postgres Db
dbAnn = defaultAnnotatedDbSettings db

runMigrations :: Connection -> IO ()
runMigrations conn = do
  tryRunMigrationsWithEditUpdate dbAnn conn
