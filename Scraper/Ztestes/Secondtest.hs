{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Ztestes.Secondtest where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Monad.IO.Class()
import Data.Text.Lazy.Encoding as LE
import Text.Taggy
import Text.Taggy.Lens
import Data.Text as T
import Data.ByteString.UTF8 (fromString)
import Control.Lens.Fold
import Control.Lens.Operators


header' = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~  ["https://www.receitasdecozina.com.br/renataalberti"]
                           & header "Origin" .~ ["http://www.receitasdecozinha.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.renataalberti.com.br/funilariaproenca"
            let fullBody          = r ^. responseBody . to LE.decodeUtf8
            let lente             = fullBody ^.. html . allNamed(only "h2")
            let links             = fullBody ^.. html . allNamed(only "a") . attributed(ix "class" . only "informacoes")
            return $ lente `mappend` links

--flavor :: Receita -> String
--flavor (Receita _ _ _ _ _ flavor) = flavor  


{-

header' = do
            r <- get "http://www.renataalberti.com.br"
            let fullBody          = r ^. responseBody . to LE.decodeUtf8
            let lente             = fullBody ^.. html . allNamed(only "h1")
            return lente
-}

getTesR :: Handler Html
getTesR = defaultLayout $ do
    setTitle "FastChef"
    rend <- liftIO $ header'
    
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
        #{Prelude.map (toMarkup False) rend}
        
    |]
    
getNotaR :: Text -> Handler Html
getNotaR inform = undefined