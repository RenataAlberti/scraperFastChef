{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Receitas.Busca where

import Foundation
import Yesod.Core
import Data.Text
import Yesod.Form
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Scraper.General
import Scraper.Services.CyberCook


postBuscaR :: Handler Html
postBuscaR = do
    ((res', widget), enctype) <- runFormPost form
    let title = "Resultado da Busca"
    case res' of
        FormSuccess res -> do
            case (buscaCampo3 res) of
                Nothing -> liftIO (searchCyberCook $ unpack " +") >>= \y -> newLayout title
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
                            ^{footer}
                        |]
                Just x -> do
                    cyberCook <- liftIO (searchCyberCook $ unpack x)
                    newLayout title
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
                                <h1> #{title} - #{x} </h1>
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