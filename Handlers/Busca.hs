{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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

postBuscaR :: Handler Html
postBuscaR = do
    ((res', widget), enctype) <- runFormPost form
    case res' of
        FormSuccess res -> do
            case (buscaCampo3 res) of
                Nothing -> liftIO (CC.q $ unpack " +") >>= \y -> defaultLayout $ do
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
                    cyberCook <- liftIO (CC.q $ unpack x)
                    allRecipes <- liftIO (AR.texto $ unpack x)
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
                    		var copyrightAr = document.getElementsByTagName("p");
                    		// var copyrightRdh = document.getElementsByTagName("span");
                    		var copyrightCc = document.getElementsByClassName("mt10 grey--dark txt-small");
                    		var elementos = document.getElementsByClassName('card--half-image__image');
                            var atributosDataPagespeed = [];
                    		for(var i = 0; i < copyrightAr.length; i++){
                    			if (copyrightAr[i].getAttribute("class") === "author"){
                    			    copyrightAr[i].innerHTML = "Fonte: <a href='http://allrecipes.com.br' title='allrecipes'> All Recipes </a>";
                    			};
                    		};
                    		for(var i = 0; i < copyrightCc.length; i++){
                    		   copyrightCc[i].innerHTML = "Fonte: <a href='https://cybercook.uol.com.br' title='cybercook'> CyberCook </a>"; 
                    		};
                    		/*for(var i = 0; i < copyrightRdh.length; i++){
                    			if (copyrightRdh[i].getAttribute("class") === "author"){
                    			    copyrightRdh[i].innerHTML = "Fonte: <a href='https://www.receitasdehoje.com.br' title='receitas-de-hoje'> Receitas de Hoje </a>";
                    			};
                    		};*/
                            for (var i = 0; i < elementos.length; i++){
                                atributosDataPagespeed[i] = elementos[i].firstChild;
                            };
                            for (var i = 0; i < atributosDataPagespeed.length; i++){
                                atributosDataPagespeed[i].setAttribute('src', atributosDataPagespeed[i].getAttribute('data-pagespeed-lazy-src'));
                            
                            };
                            for (var i = 0; i < atributosDataPagespeed.length; i++){
                                atributosDataPagespeed[i].removeAttribute('data-pagespeed-lazy-src');
                            };
                            var x = document.getElementsByClassName("list");
                            while(x.length >= 2){y = document.getElementsByClassName("list")[((x.length) - 1)]; y.parentNode.removeChild(y);}
                            
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
                            <h1> Resultados da Busca </h1>
                            #{Prelude.map (toMarkup False) cyberCook}
                            #{Prelude.map (toMarkup False) allRecipes}
                    |]
        _ -> redirect  HomeR