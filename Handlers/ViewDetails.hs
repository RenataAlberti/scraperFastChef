{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards, MultiParamTypeClasses  #-}

module Handlers.ViewDetails where

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
import qualified Text.Blaze.Html as TBH
import Data.Text.Lazy.Encoding
import Data.Text (pack, unpack)
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)

getViewDetailsR :: String -> Handler Html
getViewDetailsR x = do
        ((res', widget), enctype) <- runFormPost form
        defaultLayout $ do
            receita <- liftIO $ detalhe' x
            
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
                    <div id="containerview">
                        <h1>  #{h1 receita} </h1>
                        <aside>
                                <img src=#{imagem receita} alt="pizza-imagem-principal" class="img-receita">            
                        <div>  
                            <dl>
                                <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                                    <dd> 6 porções <dd><br>
                                <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                                    <dd>25 min </dd><br>
                                <dt><span class="margin-right"><i class="fa fa-copyright" aria-hidden="true"></i></span>  Fonte: </dt>
                                    <dd> <a href="" title="link-cybercook">  </a></dd>
                            
                            <h3> Ingredientes </h3>
                            $forall row <- (ingredientes receita)
                                $maybe h2 <- (h3 $ row)
                                    <h2> #{pack h2}
                                $forall ingred <- (lista $ row)
                                    <p> #{pack ingred}
                            
                        
                            <h3> Modo de preparo </h3>
                            <p> Frite a linguiça em uma panela, com uma colher (sopa) de azeite-de-oliva </p>
                            <p> Em um refratário redondo, untado, acomode a massa cozida, cubra com molho de tomate , linguiça, queijo mussarela, rodelas de tomate, folhas de manjericão e polvilhe orégano </p>
                            <p> Leve ao forno preaquecido, em temperatura média, só para derreter o queijo </p>
                            
                            <h3> Informações adicionais </h3>
                            <p> Se não quiser, não frite a linguicinha, apenas misture no molho de tomate. </p>
            |]