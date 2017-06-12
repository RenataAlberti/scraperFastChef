{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Scraper.Busca.Receita where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Network.Wreq
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, children)
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import qualified Data.List as DL
import Data.Text (Text, append, pack, unpack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Blaze.ByteString.Builder (toByteString)

-- scraper buscas    
header2 x = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess $ constructUrl x
            let fullBody = r ^. responseBody . to LE.decodeLatin1
            -- let link = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "gleft")
            let link = (fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "gleft") . allNamed(only "div") . attributed(ix "class" . only "listing box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix") . allNamed(only "li") . attributed(ix "class" . only "box-hover") . allNamed(only "a"))
            return $ link

urlDireto = unpack "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"

comLet = do
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
            let fullBody = (r ^. responseBody . to LE.decodeUtf8)
            let ingredientes = (fullBody ^.. html . allNamed(only "div") . attributed (ix "id" . only "info-user"))
            return $  ingredientes

header' x = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess $ constructUrl x
            let fullBody = r ^. responseBody . to LE.decodeLatin1
            let link = (fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "listing box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix") . allNamed(only "li") . attributed(ix "class" . only "box-hover") . allNamed(only "a") . attr "href" . _Just) -- NOME, PORCAO E RENDIMENTO DAS RECEITAS
            let lente = (fullBody ^.. html . allAttributed(ix "class" . only "listing") . allNamed(only "a") . attr "href" . _Just) -- LINKS DAS RECEITAS
            let imgm = (fullBody ^.. html . allNamed(only "span") . attributed (ix "class" . only "photo-holder") . allNamed(only "img") . attributed(ix "class" . only "photo") . attr "src" . _Just)
            return $ DL.concat [link, [(pack "---------------LINKS-------")], lente, [(pack "---------------IMGS-------")], imgm] 

scrapDirect x = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess $ constructDirectUrl x
            let fullBody = r ^. responseBody . to LE.decodeLatin1
            let link = (fullBody ^.. html . allNamed(only "h1") . contents) -- NOME, PORCAO E RENDIMENTO DAS RECEITAS
            let lente = (fullBody ^.. html . allAttributed(ix "class" . only "listing box") . allNamed(only "a") . attr "href" . _Just) -- LINKS DAS RECEITAS
            let imgm = (fullBody ^.. html . allNamed(only "time") . contents)
            return $ DL.concat [(DL.take 80 link), [(pack "---------------LINKS-------")], (DL.take 10 lente), [(pack "---------------IMGS-------")], imgm] 

data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "http://www.tudogostoso.com.br/busca.php" `append`
    TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) params))
    
constructUrl x = unpack $ render SomePage [(pack "q", pack x)]

constructDirectUrl x = "http://www.tudogostoso.com.br" ++ x