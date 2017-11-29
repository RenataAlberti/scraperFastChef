{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod
import Yesod.Core
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent


getListarFavR :: Handler Html
getListarFavR = undefined

postListarFavR :: Handler Html
postListarFavR = undefined

postSalvarFavR :: Handler Html
postSalvarFavR = do
    ((result, _), _) <- runFormPost formEdit
    case result of
        FormSuccess (nome, url, urlimg, fonte) -> do
            Just uId <- lookupSession "_ID"
            Just (Entity uid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ uId)] []            
            usuLR <- runDB $ insertBy (Favoritos uid nome url urlimg fonte)
            case usuLR of
              _ -> redirect HomeR
        _ -> redirect HomeR                
    

getEditarFavR :: FavoritosId -> Handler Html
getEditarFavR favid = undefined

postEditarFavR :: FavoritosId -> Handler Html
postEditarFavR favid = undefined

postExcluirFavR :: FavoritosId -> Handler Html
postExcluirFavR favid = undefined

