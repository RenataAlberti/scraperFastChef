{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, MultiParamTypeClasses  #-}

module Handlers.Jsonnaveia where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad
import Scraper.Padrao
import Utils.WidgetResultadoBusca
import Utils.SettingsForm
import Yesod.Form
import Scraper.Busca.CyberCook
import Text.Taggy 
import Text.Taggy.Lens 


getJsonnaveiaR :: Handler Html
getJsonnaveiaR = do
    (widget, enctype) <- generateFormPost form
    defaultLayout $ do
        infor <- liftIO $ detalhe' "receita-de-fatias-de-tender-ao-molho-de-laranja-e-mel-r-3-12617.html"
        setTitle "FastChef"
       
        toWidgetHead[hamlet|
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <meta charset="UTF-8">
        |]
    
        -- Adicionando o FontAwesome
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    
        -- Adicionando a folha de estilos
        addStylesheet $ StaticR css_estilo_css
    
        -- Adicionando o jquery via CDN
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
                    <div>
                        <p> 
                        #{h1 infor}
                            
        |]