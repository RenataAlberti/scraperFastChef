{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Ztestes.Boott where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import Control.Monad.IO.Class()
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, allNamed, attributed, content, children)
import Text.Taggy.DOM
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import qualified Data.List as DL
import Data.Text as T
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack, unpack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE

urlDireto = unpack "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"

header' func = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess urlDireto
            return $ r ^.. responseBody . to LE.decodeUtf8 . html . func
            
getBoottR :: Handler Html
getBoottR = defaultLayout $ do
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
        <h1> Boott </h1>
    |]


curlIt = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.tudogostoso.com.br/receita/23-bolo-de-cenoura.html"
            return $ r ^.. responseBody . to LE.decodeUtf8 . html 