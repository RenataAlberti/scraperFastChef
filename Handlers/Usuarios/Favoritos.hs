{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod
import Yesod.Core
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Data.Monoid
import Control.Lens
import Text.Read

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }
    deriving Show
    
data Prefav = Prefav
    { prefavNome   :: Text
    , prefavUrl    :: Text
    , prefavUrlimg :: Text
    , prefavFonte  :: Text
    }deriving Show
    
getListarFavR :: Handler Html
getListarFavR =  do
    fId <- lookupSession "_USER"
    case fId of
        Just str -> do
            Just (Entity fid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []
            pub <- runDB $ selectList [FavoritosUsuarioId ==. fid] [Asc FavoritosNomefavoritos]
            newLayout "deu certo" $ do
                [whamlet|
                    <div id="container">
                        <h3>Glória a Deussss!!!! Hora da pausa pra palavra!
                        $forall (Entity favid fav) <- pub
                            <p>#{favoritosNomefavoritos fav}
                            <p>#{favoritosUrl fav}
                            <p>#{favoritosUrlimg fav}
                            <p>#{favoritosFonte fav}
                |]
        Nothing -> redirect HomeR
        
postSalvarFavR :: Handler Html
postSalvarFavR = do
    person <- runInputPost $ Prefav
                <$> ireq textField "nome"
                <*> ireq textField "url"
                <*> ireq textField "urlimg"
                <*> ireq textField "fonte"
    Just uId <- lookupSession "_USER"
    Just (Entity uid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ uId)] []            
    usuLR <- runDB $ insert (Favoritos uid (prefavNome person) (prefavUrl person) (prefavUrlimg person) (prefavFonte person))
    redirect HomeR

getEditarFavR :: Handler Html
getEditarFavR = undefined

postEditarFavR :: Handler Html
postEditarFavR = undefined

postExcluirFavR :: FavoritosId -> Handler Html
postExcluirFavR favid = undefined
