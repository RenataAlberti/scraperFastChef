{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables,
             GeneralizedNewtypeDeriving, TemplateHaskell, GADTs,
             TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}

module Handlers.Busca where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text(Text, pack, unpack)
import Yesod.Form
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Utils.WidgetResultadoBusca
import Utils.SettingsForm
import Scraper.Ztestes.Boott as B
import Scraper.Busca.Receita as R
import Scraper.Busca.AllRecipes as AR
import Scraper.Busca.CyberCook as CC
import Scraper.Busca.ReceitasDeHoje as RDH
import Text.Taggy 
import Text.Taggy.Lens 
import Scraper.Padrao


postBuscaR :: Handler Html
postBuscaR = do
    ((res', widget), enctype) <- runFormPost form
    case res' of
        FormSuccess res -> do
            case (buscaCampo3 res) of
                Nothing -> liftIO (CC.haha $ unpack " +") >>= \y -> defaultLayout $ do
                    setTitle "FastChef - Nenhum resultado :("
                    toWidgetHead[hamlet|
                        <meta http-equiv="X-UA-Compatible" content="IE=edge">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                    |]
                    
                    -- Adicionando o FontAwesome
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
                    
                    -- Adicionando a folha de estilos
                    addStylesheet $ StaticR css_estilo_css
                    
                    -- Adicionando o jquery via CDN
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"
                    
                    toWidget[julius|
                        
                    |]
                    [whamlet|
                        <header> 
                            <nav id="menu">
                                <ul>
                                    <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                                    <li>
                                        <form method=post action=@{BuscaR} enctype=#{enctype}>
                                            ^{widget}
                                            <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                        <div id="container">    
                            <h1> Nenhum Resultado! :( </h1>
                            <a href="@{HomeR}" title="voltar"> Voltar para o início </a>
                    |]
                Just x -> do
                    cyberCook <- liftIO (CC.haha $ unpack x)
                    defaultLayout $ do
                    setTitle "FastChef - Resultados da Busca"
                    toWidgetHead[hamlet|
                        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1">
                    |]
                    
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
                
                    addStylesheet $ StaticR css_estilo_css
                    
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"
                    
                    toWidget[julius|
                        window.onload=function(){
                            
                        }
                    |]
                    
                    [whamlet|
                        <header> 
                            <nav id="menu">
                                <ul>
                                    <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                                    <li>
                                        <form method=post action=@{BuscaR} enctype=#{enctype}>
                                            ^{widget}
                                            <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                        <div  id="container">
                            <h1> Resultados da Busca - #{x} </h1>
                            $forall cc <- cyberCook
                                <div class="row recipe">
                                    <h2> #{titulo cc} </h2>
                                    <img src="#{img cc}" alt="#{titulo cc}" class="img-thumb">
                                    <dl>
                                        <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                        <dd> 6 porções <dd><br/>
                                        <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                        <dd>25 min </dd><br/>
                                        <dt><span class="margin-right"><i class="fa fa-external-link" aria-hidden="true"></i></span>  Fonte: </dt>
                                        <dd> <a href="#{fonteurl (lincopy cc)}" title="#{show $ nm (lincopy cc)}"> #{show $ nm (lincopy cc)} </a> </dd>
                                    <div class="btnlink">
                                        <a href="@{ViewDetailsR (lin cc)}" title="#{titulo cc}" class="linkbtn"> Ver receita </a>
                        <footer>
                            <p> Colossenses 3.17 </p>
                            <p> Desenvolvido por: Renata Alberti </p>
                    |]
        _ -> redirect  HomeR