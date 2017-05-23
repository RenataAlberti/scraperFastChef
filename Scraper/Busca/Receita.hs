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
            let link = (fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "gleft") . allNamed(only "div") . attributed(ix "class" . only "listing box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix") . allNamed(only "li") . attributed(ix "class" . only "box-hover") . allNamed(only "a") . attr "href" . _Just) -- NOME, PORCAO E RENDIMENTO DAS RECEITAS
            let link2 = (fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix") . allNamed(only "li") . attributed(ix "class" . only "box-hover") . allNamed(only "a") . attr "href" . _Just)     
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

-- [1 | _ <- xs]  
  
-- insertList xs e 0 = e:xs
-- insertList (x:xs) e n = x:(insert_list xs e (n-1))

--removeList (x:xs) e 0 = xs
--removeList (x:xs) e n = x:(remove_list xs e (n-1)) 

-- (DL.concat [(DL.take 1 link), (DL.take 1 lente), (DL.take 6 link), (DL.take 7 link)])data MyRoute = SomePage
data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "http://www.tudogostoso.com.br/busca.php" `append`
    TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) params))
    
constructUrl x = unpack $ render SomePage [(pack "q", pack x)]


constructDirectUrl x = "http://www.tudogostoso.com.br" ++ x

{-
data MyRoute = SomePage

render :: MyRoute -> Text -> [(Text, Text)] -> Text
render SomePage urlFixo params = urlFixo `append`
    TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) params))

constructUrl y x = unpack $ render SomePage y [(pack "q", pack x)]

-- Scraper Busca    
header' x = do
            let tudoGostoso = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            
            let cyberCook = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=cybercook&rlz=1C1AVFB_enBR731BR732&oq=cybercook&aqs=chrome..69i57j0l5.1755j0j9&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
           
            let receitasIg = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receitas+ig&rlz=1C1AVFB_enBR731BR732&oq=receitas+ig&aqs=chrome.0.69i59j0l5.2426j0j7&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            
            -- Requisicoes via wreq
            -- TudoGostoso: tg | CyberCook: cc | ReceitasIg: ri           
            tg <- S.withSession $ \sess -> do
                S.getWith tudoGostoso sess $ constructUrl (pack "http://www.tudogostoso.com.br/busca.php") x
            
            cc <- S.withSession $ \sess -> do
                S.getWith cyberCook sess $ constructUrl (pack "https://cybercook.uol.com.br/resultado.php") x
            
            ri <- S.withSession $ \sess -> do
                S.getWith receitasIg sess $ constructUrl (pack "http://receitas.ig.com.br/busca/") x
            
            -- Respostas
            -- TudoGostoso: fullBodyTg | CyberCook: fullBodyCc | ReceitasIg: fullBodyRi    
            let fullBodyTg = tg ^. responseBody . to LE.decodeLatin1
            
            let fullBodyCc = cc ^. responseBody . to LE.decodeLatin1
            
            let fullBodyRi = ri ^. responseBody . to LE.decodeLatin1
            
            -- Lentes
            -- Title: Titulo | Link: Link da Receita | Img: Caminho da Imagem | Rend: Rendimento/Porcao | Time: Tempo de preparo | Copy: Copyright
            let tgTitle = (fullBodyTg ^.. html . allNamed(only "div") . attributed(ix "class" . only "listing box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix") . to universe . traverse . contents)
            --let tgLink  = ()
            --let tgImg   =
            --let tgRend  =
            --let tgTime  =
            --let tgCopy  =
            
            let ccTitle = (fullBodyCc ^.. html . allNamed(only ""))
            --let ccLink  =
            --let ccImg   =
            --let ccRend  =
            --let ccTime  =
            --let ccCopy  =
            
            let riTitle = 
            --let riLink  =
            --let riImg   =
            --let riRend  =
            --let riTime  =
            --let riCopy  =
            
            return $ DL.concat [[(pack "--------------- TudoGostoso -------")], tgTitle, [(pack "--------------- CyberCoook -------")], ccTitle, [(pack "--------------- Receitas IG -------"), riTitle]] 
-}