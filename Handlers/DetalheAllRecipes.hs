{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.DetalheAllRecipes where

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
import Text.Taggy 
import Text.Taggy.Lens 


getArDetalheR :: Handler Html
getArDetalheR = do
            ((res', widget), enctype) <- runFormPost form
            defaultLayout $ do
            detalhescc <- liftIO $ AR.detalhe
            setTitle "FastChef - Resultados da Busca"
            toWidgetHead[hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
            |]
            
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
        
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
                <div  id="container">
                    <h1> Detalhes da Receita </h1>
                    <hr>
                    <hr>
                    #{Prelude.map (toMarkup False) detalhescc}
            |]