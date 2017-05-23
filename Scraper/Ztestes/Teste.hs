{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Ztestes.Teste where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import Control.Monad.IO.Class()
import Data.Text.Lazy.Encoding as LE
import Text.Taggy()
import Text.Taggy.Lens as TL
import qualified Network.Wreq.Session as S
import Data.Text as T

rendimento :: ((Text -> Const Text Text) -> Node -> Const Text Node) -> ((Text -> Const Text Text) -> Node -> Const Text Node) -> IO Text
rendimento x y = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~  ["http://www.tudogostoso.com.br/busca.php?q=bolo+de+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.tudogostoso.com.br/receita/23-bolo-de-cenoura.html"
            s <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.tudogostoso.com.br/receita/23-bolo-de-cenoura.html"
            return $ T.concat [r ^. responseBody . to LE.decodeUtf8 . html . x, "---------", s ^. responseBody . to LE.decodeUtf8 . html . y]

raspagem :: (Text -> Const Text Text) -> Node -> Const Text Node
raspagem = allNamed( only "div") . TL.contents

rasp :: (Text -> Const Text Text) -> Node -> Const Text Node
rasp = allNamed( only "h1") . TL.contents

getTesteR :: Handler Html
getTesteR = defaultLayout $ do
    setTitle "FastChef"
    rend        <- liftIO $ rendimento raspagem rasp
    
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
                    <li> ---------------------------------------------
                    <li> <span class="bold"> Rendimento: </span> #{rend}
                    <li> ---------------------------------------------
    |]