{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Static()
import Yesod.Form
import Widgets.SettingsForm
import Yesod.Auth
import Widgets.PageGenericContent
import Scraper.General
import Scraper.Services.CyberCook
import Scraper.Services.AllRecipes
import Data.Text

getHomeR :: Handler Html
getHomeR = do 
    ((a, widget), enctype) <- generateFormGet form
    maid <- maybeAuthId
    allRecipes <- liftIO (searchAllRecipes $ unpack "tomate")
    cyberCook <- liftIO (searchCyberCook $ unpack "tomate")
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
    newLayout ("FastChef")
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
                <h1> Início </h1>
                $forall ar <- allRecipes
                    <div class="row recipe">
                        <a href="@{ViewArR (lin ar)}" title="#{titulo ar}">
                            <h2> #{removeElements 26 (titulo ar)} </h2>
                            <img src="#{img ar}" alt="#{titulo ar}" class="img-thumb">
                            <dl>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                <dd> - porções </dd><br/>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                <dd> - </dd><br/>
                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                <dd> <a href="#{fonteurl (lincopy ar)}" title="#{show $ nm (lincopy ar)}"> #{show $ nm (lincopy ar)} </a> </dd>
                            <div class="btnlink">
                                <a href="@{ViewArR (lin ar)}" title="#{titulo ar}" class="linkbtn"> Ver receita </a>
                $forall cc <- cyberCook
                    <div class="row recipe">
                        <a href="@{ViewDetailsR (lin cc)}" title="#{titulo cc}">
                            <h2> #{titulo cc} </h2>
                            <img src="#{img cc}" alt="#{titulo cc}" class="img-thumbb">
                            <dl>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                <dd> - porções </dd><br/>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                <dd> - </dd><br/>
                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                <dd> <a href="#{fonteurl (lincopy cc)}" title="#{show $ nm (lincopy cc)}"> #{show $ nm (lincopy cc)} </a> </dd>
                            <div class="btnlink">
                                <a href="@{ViewDetailsR (lin cc)}" title="#{titulo cc}" class="linkbtn"> Ver receita </a>
            ^{footer}
        |]