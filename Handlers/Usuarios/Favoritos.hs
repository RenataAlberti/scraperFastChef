{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod
import Widgets.SettingsForm
import Widgets.PageGenericContent


getListarFavR :: Handler Html
getListarFavR = undefined

postListarFavR :: Handler Html
postListarFavR = undefined

postSalvarFavR :: Handler Html
postSalvarFavR = undefined

getEditarFavR :: FavoritosId -> Handler Html
getEditarFavR favid = undefined

postEditarFavR :: FavoritosId -> Handler Html
postEditarFavR favid = undefined

getExcluirFavR :: FavoritosId -> Handler Html
getExcluirFavR favid = undefined

postExcluirFavR :: FavoritosId -> Handler Html
postExcluirFavR favid = undefined

