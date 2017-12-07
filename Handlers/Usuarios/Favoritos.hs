{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Favoritos where

import Foundation
import Yesod
import Yesod.Core
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Text.Blaze
import Yesod.Auth
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2


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
            Just (Entity fid favs) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []            
            favinsert <- runDB $ 
                            insert  (Favoritos fid (prefavNome favoritos)
                                    (prefavUrl favoritos) 
                                    (prefavUrlimg favoritos) 
                                    (prefavUrlfonte favoritos) 
                                    (prefavNomefonte favoritos))
            setMessage[shamlet|
                <p> Salvo com sucesso! </p>
            |]
            redirect (ViewDetailsR (unpack $ prefavUrl favoritos))


getExcluirFavR :: FavoritosId -> Handler Html
getExcluirFavR favid = do
    fav <- runDB $ deleteWhere [FavoritosId ==. favid]
    redirect ListarFavR
    

getListarFavR :: Handler Html
getListarFavR =  do
    ((a, widget), enctype) <- generateFormGet form
    maid <- maybeAuthId
    fId <- lookupSession "_USER"
    case fId of
        Just str -> do
            Just (Entity fid prophet) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []
            favs <- runDB $ selectList [FavoritosUsuarioId ==. fid] [Asc FavoritosNomefavoritos]
            newLayout "Favoritos" $ do
                [whamlet|
                    <header> 
                        <nav id="menu">
                            <div id="naveg">
                                <ul>
                                    $maybe _ <- maid
                                        <li> 
                                            <a href=@{ListarFavR} title="favoritos"> Favoritos
                                        <li> 
                                            <a href=@{LoogoutR} title="logout"> Logout
                                    $nothing
                                        <li> 
                                            <a href=@{RegisterR} title="cadastro"> Cadastro
                                        <li> 
                                            <a href=@{LooginR} title="login"> Login
                            ^{menu BuscaR enctype widget}
                    <div id="container">
                        <h1> Favoritos </h1>
                        $maybe _ <- maid
                            $forall (Entity favid fav) <- favs
                                <div class="row recipe">
                                    <a href=@{ViewArR (unpack (favoritosUrl fav))} title="#{favoritosNomefavoritos fav}">
                                        <h2> #{favoritosNomefavoritos fav} </h2>
                                        <a href=@{ExcluirFavArR (favid)} title="Excluir"> <i class="fa fa-trash" aria-hidden="true"></i> Excluir </a>
                                        <img src="#{favoritosUrlimg fav}" alt="#{favoritosNomefavoritos fav}" class="img-thumb">
                                        <dl>
                                            <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                                <dd> 6 porções </dd><br/>
                                            <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                                <dd>25 min </dd><br/>
                                            <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                                <dd> <a href="#{favoritosUrlfonte fav}" title="#{favoritosNomefonte fav}"> #{favoritosNomefonte fav} </a> </dd>
                                        <div class="btnlink">
                                            $if (favoritosNomefonte fav) == "AllRecipes"
                                                <a href="@{ViewArR (unpack (favoritosUrl fav))}" title="#{favoritosNomefavoritos fav}" class="linkbtn"> Ver receita </a>
                                            $else
                                                <a href="@{ViewDetailsR (unpack (favoritosUrl fav))}" title="#{favoritosNomefavoritos fav}" class="linkbtn"> Ver receita </a>
                        $nothing
                            <p class="alert"> Hey! Você precisa fazer login.
                            <p>Já tem cadastro? <a href=@{LooginR} title="login" class="link"> Clique aqui </a> para entrar na sua conta.
                            <p>Ainda não tem cadastro? <a href=@{RegisterR} title="cadastro" class="link"> Clique aqui </a> para se cadastrar.
                    ^{footer}
                |]
        Nothing -> redirect HomeR
        
postSalvarFavArR :: Handler Html
postSalvarFavArR = do
    favoritos <- runInputPost $ Prefav
                <$> ireq textField "nome"
                <*> ireq textField "url"
                <*> ireq textField "urlimg"
                <*> ireq textField "urlfonte"
                <*> ireq textField "nomefonte"
    fId <- lookupSession "_USER"
    case fId of
        Just str -> do
            Just (Entity fid favs) <- runDB $ selectFirst [UsuarioLoginId ==. (read . unpack $ str)] []            
            favinsert <- runDB $ 
                            insert  (Favoritos fid (prefavNome favoritos)
                                    (prefavUrl favoritos) 
                                    (prefavUrlimg favoritos) 
                                    (prefavUrlfonte favoritos) 
                                    (prefavNomefonte favoritos))
            setMessage[shamlet|
                <p> Salvo com sucesso! </p>
            |]
            redirect (ViewArR (unpack $ prefavUrl favoritos))


getExcluirFavArR :: FavoritosId -> Handler Html
getExcluirFavArR favid = do
    fav <- runDB $ deleteWhere [FavoritosId ==. favid]
    redirect ListarFavR