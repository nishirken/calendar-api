import Config (getConfig)
import Db (connectDb)
import Migrations.Migrate (migrateDb, verifyDbSchema)

main :: IO ()
main = do
  config <- getConfig ".env"
  conn <- connectDb config
  migrateDb conn
  print "Migration has been completed"
  verifyDbSchema conn
