{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Receitas.ViewAr where

import Foundation
import Yesod.Core
import Scraper.General
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Yesod.Form
import Scraper.Services.AllRecipes
import Data.Text
import Yesod.Auth
import Yesod
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2

getViewArR :: String -> Handler Html
getViewArR x = do
        ((res', widget), enctype) <- runFormPost form
        maid <- maybeAuthId
        msg <- getMessage
        newLayout ("Detalhe da Receita") $ do
            receita <- liftIO $ viewAllRecipes x
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
                <div id="containerview">
                    <h1>  #{Prelude.drop 22 (removeElements 18 (h1 receita))} </h1>
                    <aside>
                        <div>
                            <img src=#{imagem receita} alt="#{h1 receita}" class="img-receita allrecipes">            
                            <dl>
                                <dt>
                                $maybe _ <- maid
                                    $maybe mensagem <- msg
                                            #{mensagem}
                                    $nothing
                                        <form action=@{SalvarFavArR} method=post>
                                            <input type=text value=#{pack $ Prelude.drop 22 (removeElements 18 (h1 receita))} name="nome" hidden>
                                            <input type=text value=#{pack $ x} name="url" hidden>
                                            <input type=text value=#{pack $ imagem receita} name="urlimg" hidden>
                                            <input type=text value=#{pack $ (fonteurl (copyright receita))} name="urlfonte" hidden>
                                            <input type=text value=#{pack $ show $ nm (copyright receita)} name="nomefonte" hidden>
                                            <button type=submit class="favoritos">
                                                <span class="margin-right"><i class="fa fa-heart" aria-hidden="true"></i></span> Salvar nos favoritos
                                $nothing
                                    <dd></dd>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                    <dd> - porções <br>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                    <dd> - <br>
                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                    <dd> <a href="#{fonteurl (copyright receita)}" title="#{fonteurl (copyright receita)}"> #{show $ nm (copyright receita)} </a> <br>
                    <div>
                        <h2> Ingredientes </h2>
                        $forall ings <- (ingredientes receita)
                            $maybe h <- (h3 $ ings)
                                <h3> #{pack h}
                            $forall ingred <- (lista $ ings)
                                <ul>    
                                    <li> #{pack ingred}
                        <h2> Modo de preparo </h2>
                        $forall mdp <- (modopreparo receita)
                            $maybe h' <- (h3 $ mdp)
                                <h3> #{pack h'}
                            $forall ingred <- (lista $ mdp)
                                <ul>
                                    <li> #{pack ingred}
                ^{footer}
            |]