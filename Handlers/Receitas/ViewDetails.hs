{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Receitas.ViewDetails where

import Foundation
import Yesod.Core
import Scraper.General
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Yesod.Form
import Scraper.Services.CyberCook
import Data.Text
import Yesod.Auth
import Yesod
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2


getViewDetailsR :: String -> Handler Html
getViewDetailsR x = do
        ((res', widget), enctype) <- runFormPost form
        maid <- maybeAuthId
        newLayout ("Detalhe da Receita") $ do
            receita <- liftIO $ viewCyberCook x
            [whamlet|
                ^{menu BuscaR enctype widget}
                <div id="containerview">
                    <h1>  #{h1 receita} </h1>
                    <aside>
                        <div>
                            <img src=#{imagem receita} alt="#{h1 receita}" class="img-receita">            
                            <dl>
                                <dt>
                                $maybe _ <- maid
                                    <form action=@{SalvarFavR} method=post>
                                        <input type=text value=#{pack $ (h1 receita)} name="nome" hidden>
                                        <input type=text value=#{pack $ x} name="url" hidden>
                                        <input type=text value=#{pack $ imagem receita} name="urlimg" hidden>
                                        <input type=text value=#{pack $ (fonteurl (copyright receita))} name="urlfonte" hidden>
                                        <input type=text value=#{pack $ show $ nm (copyright receita)} name="nomefonte" hidden>
                                        <button type=submit class="favoritos">
                                            <span class="margin-right"><i class="fa fa-heart" aria-hidden="true"></i></span> Salvar nos favoritos
                                $nothing
                                    <dd></dd>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                    <dd> 6 porções <br>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                    <dd>25 min <br>
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
            
            