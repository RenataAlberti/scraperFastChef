{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Receitas.Busca where

import Foundation
import Yesod.Core
import Data.Text
import Yesod.Form
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Scraper.General
import Scraper.Services.CyberCook
import Scraper.Services.AllRecipes
import Yesod.Auth
import Yesod
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2
import Widgets.PageGenericContent

getBuscaR :: Handler Html
getBuscaR = do
    ((res', widget), enctype) <- runFormGet form
    maid <- maybeAuthId
    let title = "Resultado da busca"
    case res' of
        FormSuccess res -> do
            case (q res) of
                Nothing -> liftIO (searchCyberCook $ unpack " +") >>= \y -> newLayout title
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
                                <h1> Nenhum Resultado! :( </h1>
                                <a href="@{HomeR}" title="voltar"> Voltar para o início </a>
                            ^{footer}
                        |]
                Just x -> do
                    allRecipes <- liftIO (searchAllRecipes $ unpack x)
                    cyberCook <- liftIO (searchCyberCook $ unpack x)
                    maid <- maybeAuthId
                    newLayout title
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
                            <div  id="container">
                                <h1> #{title} - #{x} </h1>
                                $forall ar <- allRecipes
                                    <div class="row recipe">
                                        <a href="@{ViewArR (lin ar)}" title="#{titulo ar}">
                                            <h2> #{removeElements 26 (titulo ar)} </h2>
                                            <img src="#{img ar}" alt="#{titulo ar}" class="img-thumb">
                                            <dl>
                                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                                <dd> 6 porções </dd><br/>
                                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                                <dd>25 min </dd><br/>
                                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                                <dd> <a href="#{fonteurl (lincopy ar)}" title="#{show $ nm (lincopy ar)}"> #{show $ nm (lincopy ar)} </a> </dd>
                                            <div class="btnlink">
                                                <a href="@{ViewArR (lin ar)}" title="#{titulo ar}" class="linkbtn"> Ver receita </a>
                                $forall cc <- cyberCook
                                    <div class="row recipe">
                                        <a href="@{ViewDetailsR (lin cc)}" title="#{titulo cc}">
                                            <h2> #{titulo cc} </h2>
                                            <img src="#{img cc}" alt="#{titulo cc}" class="img-thumb">
                                            <dl>
                                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                                <dd> 6 porções </dd><br/>
                                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                                <dd>25 min </dd><br/>
                                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                                <dd> <a href="#{fonteurl (lincopy cc)}" title="#{show $ nm (lincopy cc)}"> #{show $ nm (lincopy cc)} </a> </dd>
                                            <div class="btnlink">
                                                <a href="@{ViewDetailsR (lin cc)}" title="#{titulo cc}" class="linkbtn"> Ver receita </a>
                            ^{footer}
                        |]
        _ -> redirect  HomeR