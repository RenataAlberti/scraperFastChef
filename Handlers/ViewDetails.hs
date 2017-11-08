{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.ViewDetails where

import Foundation
import Yesod.Core
import Yesod.Static
import Scraper.Padrao
import Utils.SettingsForm
import Yesod.Form
import Scraper.Busca.CyberCook
import Data.Text


getViewDetailsR :: String -> Handler Html
getViewDetailsR x = do
        ((res', widget), enctype) <- runFormPost form
        defaultLayout $ do
            receita <- liftIO $ viewCyberCook x
            
            setTitle "FastChef - Resultados da Busca"
            toWidgetHead[hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
            |]
            
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
            
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Dosis"
            
            addStylesheet $ StaticR css_estilo_css
            
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"
            
            [whamlet|
                    <header> 
                            <nav id="menu">
                                <ul>
                                    <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                                    <li>
                                        <form method=post action=@{BuscaR} enctype=#{enctype}>
                                          ^{widget}
                                         <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                    <div id="containerview">
                        <h1>  #{h1 receita} </h1>
                        <aside>
                            <div>
                                <img src=#{imagem receita} alt="pizza-imagem-principal" class="img-receita">            
                                <dl>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                    <dd> 6 porções <dd><br>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                    <dd>25 min </dd><br>
                                <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                    <dd> <a href="#{fonteurl (copyright receita)}" title="#{fonteurl (copyright receita)}">#{show $ nm (copyright receita)} </a></dd>
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
                    <footer>
                        <p> Colossenses 3.17 </p>
                        <p> Desenvolvido por: Renata Alberti </p>
                        
            |]