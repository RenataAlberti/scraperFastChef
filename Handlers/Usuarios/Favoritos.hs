{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod
import Yesod.Core
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Text.Blaze

getListarFavR :: Handler Html
getListarFavR =  do
    (widget, enctype) <- generateFormPost form
    fId <- lookupSession "_USER"
    case fId of
        Just str -> do
            Just (Entity fid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []
            favs <- runDB $ selectList [FavoritosUsuarioId ==. fid] [Asc FavoritosNomefavoritos]
            newLayout "Favoritos" $ do
                [whamlet| 
                    ^{menu BuscaR enctype widget}
                    <div id="container">
                        <h1> Favoritos </h1>
                            $forall (Entity favid fav) <- favs
                                <div class="row recipe">
                                    <a href=@{ViewDetailsR (unpack (favoritosUrl fav))} title="#{favoritosNomefavoritos fav}">
                                        <h2> #{favoritosNomefavoritos fav} </h2>
                                        <a href=@{ExcluirFavR (favid)} title="Excluir"> <i class="fa fa-trash" aria-hidden="true"></i> Excluir </a>
                                        <img src="#{favoritosUrlimg fav}" alt="#{favoritosNomefavoritos fav}" class="img-thumb">
                                        <dl>
                                            <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                                <dd> 6 porções </dd><br/>
                                            <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                                <dd>25 min </dd><br/>
                                            <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                                <dd> <a href="#{favoritosUrlfonte fav}" title="#{favoritosNomefonte fav}"> #{favoritosNomefonte fav} </a> </dd>
                                        <div class="btnlink">
                                            <a href="@{ViewDetailsR (unpack (favoritosUrl fav))}" title="#{favoritosNomefavoritos fav}" class="linkbtn"> Ver receita </a>
                    ^{footer}
                |]
        Nothing -> redirect HomeR
        
postSalvarFavR :: Handler Html
postSalvarFavR = do
    favoritos <- runInputPost $ Prefav
                <$> ireq textField "nome"
                <*> ireq textField "url"
                <*> ireq textField "urlimg"
                <*> ireq textField "urlfonte"
                <*> ireq textField "nomefonte"
    fId <- lookupSession "_USER"
    case fId of
        Just str -> do
            Just (Entity favid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []
            favo <- runDB $ selectFirst [FavoritosUsuarioId ==. favid, FavoritosUrlfonte ==. (prefavUrlfonte favoritos)] []
            case ((Prelude.length favo) > 0) of
                True -> do
                    redirect LooginR
                False -> do
                    redirect HomeR

getExcluirFavR :: FavoritosId -> Handler Html
getExcluirFavR favid = do
    fav <- runDB $ deleteWhere [FavoritosId ==. favid]
    redirect ListarFavR