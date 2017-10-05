{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Scraper.Busca.CyberCook where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Scraper.Padrao

q x = do
        let cyberCook = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                       & header "Accept" .~ ["text/html, */*"]
                       & header "X-Requested-With" .~ ["XMLHttpRequest"]
                       & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                       & header "Accept-Encoding" .~ ["gzip, deflate"]
                       & header "Referer" .~ ["https://www.google.com.br/search?q=cybercook&rlz=1C1AVFB_enBR731BR732&oq=cybercook&aqs=chrome..69i57j0l5.1755j0j9&sourceid=chrome&ie=UTF-8"]
                       & header "Origin" .~ ["http://www.google.com.br"]
                       & header "Connection" .~ ["keep-alive"]
        r <- S.withSession $ \sess -> do
            S.getWith cyberCook sess $ constructUrl CyberCook x
        let fullBody = r ^. responseBody . to LE.decodeUtf8
        let lenteDiv = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "list") 
        let filterRating = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "card__score txt-small") . name /= Just "div"))) lenteDiv
        return filterRating

-- ix "class" . only "card--half-image--without-image"
-- https://cybercook.uol.com.br/resultado.php?q=arroz
-- https://cybercook.uol.com.br/resultado.php?q=caldo+de+feijão