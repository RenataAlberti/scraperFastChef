{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}

module Foundation where

import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative()
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool)

-- static
staticFiles "static"

-- importanto o getStatic para coisas estaticas e connPool para conexao do banco
data App = App {getStatic :: Static, connPool :: ConnectionPool}

-- criacao do banco
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
    loginId         LoginId
    nome            Text Maybe
    email           Text
    senha           Text
    UniqueUsuario  email
    deriving Show

Login
    email    Text
    senha    Text
    UniqueLogin email
    deriving Show

Favoritos
    usuarioId       UsuarioId
    nomefavoritos   Text
    url             Text
    urlimg          Text
    fonte           Text
    deriving Show
|]

-- arquivo routes
mkYesodData "App" $(parseRoutesFile "routes")

newLayout :: Text -> Widget -> Handler Html
newLayout title widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{title}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    <meta name="viewport" content="width=device-width, initial-scale=1"/>
                        <meta http-equiv="X-UA-Compatible" content="IE=edge">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                        <link rel="shortcut icon" type="image/png" href=@{StaticR img_miniLogo_png}/>
                        <link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" rel="stylesheet" integrity="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN" crossorigin="anonymous">
                        <link href=@{StaticR css_estilo_css} rel="stylesheet">
                        <script src="https://code.jquery.com/jquery-3.2.1.min.js" integrity="sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4=" crossorigin="anonymous"></script>
                <body>
                    <article>
                        ^{pageBody pc}
        |]

-- sei la pra que eh
instance Yesod App

-- Formulario
type Form a = Html -> MForm Handler (FormResult a, Widget)


-- Formulario
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password>
            <div>Confirm:
            <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }

-- banco
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool
        
