{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Static()
import Yesod.Form
import Widgets.SettingsForm
{-
funcWidget :: Widget
funcWidget x = do
        toWidget [hamlet|
           <p>    
   |]
   
-- "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"

{-
getDetalheR :: Text -> Text -> Handler Html
getDetalheR x y= defaultLayout $ do 
        detalhe <- liftIO $ comLet
        setTitle "FastChef"
        
        [whamlet|
           #{Prelude.map (toMarkup False) detalhe}
        |]
        --detalhe <- liftIO $ directLink 
         -- #{Prelude.map (toMarkup False) detalhe}
         
getDetalheR :: Handler Html
getDetalheR = do
        (widget, enctype) <- generateFormPost form
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
                        <h1> Detalhe da Receita </h1>
          
                
                |]
-}
 -}      

getHomeR ::Handler Html
getHomeR = do 
    (widget, enctype) <- generateFormPost form
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
    newLayout ("FastChef")
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
                <div class="row recipe">
                    <a href="" title="pizza-link">
                        <h2> Macarrão sabor pizza </h2>
                        <img src=@{StaticR img_pizza_jpg} alt="pizza-imagem-principal" class="img-thumb">
                        <dl>
                            <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                            <dd> 6 porções </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                            <dd>25 min </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-copyright" aria-hidden="true"></i></span>  Fonte: </dt>
                            <dd> TudoGostoso </dd>
            <footer>
                        <p> Colossenses 3.17 </p>
                        <p> Desenvolvido por: Renata Alberti </p>
        |]