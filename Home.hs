{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text(Text, pack, unpack)
import Yesod.Form
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Utils.WidgetResultadoBusca
import Utils.SettingsForm
import Handlers.Busca
import Scraper.Busca.Receita
{-
funcWidget :: Widget
funcWidget x = do
        toWidget [hamlet|
           <p>    
   |]
 -}      

getHomeR ::Handler Html
getHomeR = do 
    (widget, enctype) <- generateFormPost form
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
    defaultLayout $ do
        setTitle "FastChef"
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
                <h1> Início </h1>
                <section>
                    <a href=@{BoottR} title="pizza-link">
                        <h2> Macarrão sabor pizza </h2>
                        <img src=@{StaticR img_pizza_jpg} alt="pizza-imagem-principal" class="img-thumb">
                        <dl>
                            <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                            <dd> 6 porções </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                            <dd>25 min </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-copyright" aria-hidden="true"></i></span>  Fonte: </dt>
                            <dd> TudoGostoso </dd>
        |]