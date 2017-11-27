{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}

module Foundation where

import Yesod
import Yesod.Core
import Yesod.Static
import Control.Applicative()
import Data.Text()
import Yesod.Form
import Data.Text
import Scraper.General
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)



-- static
staticFiles "static"

-- importanto o getStatic para coisas estaticas e connPool para conexao do banco
data App = App {getStatic :: Static, connPool :: ConnectionPool}

-- criacao do banco
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Login
    nome        Text
    senha       Text
    email       Text
    deriving Show

Favoritos
    loginId     LoginId
    categoriaId CategoriaId
    nome        Text
    url         Text
    urlimg      Text
    deriving Show

Categoria
    nome        Text
    deriving Show
|]

-- arquivo routes
mkYesodData "App" $(parseRoutesFile "routes")

-- sei la pra que eh
instance Yesod App

-- Formulario
type Form a = Html -> MForm Handler (FormResult a, Widget)

-- Formulario
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
-- banco
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool
