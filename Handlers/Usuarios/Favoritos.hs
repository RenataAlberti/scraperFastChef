{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod.Core
import Data.Text


getListarFavR :: Handler Html
getListarFavR = undefined

postListarFavR :: Handler Html
postListarFavR = undefined

getEditarFavR :: FavoritosId -> Handler Html
getEditarFavR favid = undefined

postEditarFavR :: FavoritosId -> Handler Html
postEditarFavR favid = undefined

getExcluirFavR :: FavoritosId -> Handler Html
getExcluirFavR favid = undefined

postExcluirFavR :: FavoritosId -> Handler Html
postExcluirFavR favid = undefined