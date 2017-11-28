{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=d8h79a3ju7m2i2 host=ec2-107-21-109-15.compute-1.amazonaws.com user=bdtiqjhebmkuhm password=2dad2bddfc9f7b7d4d29b7cf08710dabf09c1f2fa57ad515b39a6c3af729354b port=5432"

main :: IO ()
main = do
    runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    static@(Static settings) <- static "static"
    warp 8080 (App static pool)

{-
do
static@(Static settings) <- static "static"
warp 8080 (App static)
       
-}