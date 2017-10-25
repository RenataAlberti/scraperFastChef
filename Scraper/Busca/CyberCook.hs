{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Scraper.Busca.CyberCook where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Scraper.Padrao
import Data.Tree
import Data.Tree.Lens
import Data.String.UTF8
import qualified Text.Taggy.Lens as TGL
import qualified Data.Text.Encoding as DTE
import qualified Text.Taggy.Renderer as Renderer
import qualified Text.XML.Cursor as XML
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Yesod.Static()
import Yesod.Core
import Foundation

{-
http://www.renataalberti.com.br/
-}

q x = do
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                   & header "Accept" .~ ["text/html, */*"]
                   & header "X-Requested-With" .~ ["XMLHttpRequest"]
                   & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                   & header "Accept-Encoding" .~ ["gzip, deflate"]
                   & header "Referer" .~ ["https://www.google.com.br/search?q=cybercook&rlz=1C1AVFB_enBR731BR732&oq=cybercook&aqs=chrome..69i57j0l5.1755j0j9&sourceid=chrome&ie=UTF-8"]
                   & header "Origin" .~ ["http://www.google.com.br"]
                   & header "Connection" .~ ["keep-alive"]
    r <- S.withSession $ \sess -> do
        S.getWith header' sess $ constructUrl CyberCook x
    let fullBody     = r ^. responseBody . to LE.decodeUtf8
    let lenteDiv     = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "list")
    let filterDiv    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "grid-lg-9") . name /= Just "div"))) lenteDiv
    let filterImg    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "card__flag") . name /= Just "div"))) filterDiv
    let filterRating = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "card__score txt-small") . name /= Just "div"))) filterImg
    return filterRating


detalhe' = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=cybercook&rlz=1C1AVFB_enBR731BR732&oq=cybercook&aqs=chrome..69i57j0l5.1755j0j9&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess "https://cybercook.uol.com.br/receita-de-bolo-de-chocolate-r-12-1603.html"
            let fullBody      = r ^. responseBody . to LE.decodeUtf8
            let h1            = fullBody ^.. html . allNamed(only "h1")
            let tempPrepRend  = fullBody ^.. html . allNamed(only "p") . attributed(ix "class" . only "font-serif pb20")
            let img           = fullBody ^.. html . allNamed(only "img") . attributed(ix "class" . only "photo")
            let ingredientes  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "printable")
            let filterIng1    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "id" . only "banner-1x1-4-area") . name /= Just "div"))) ingredientes
            let filterIng2    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "id" . only "banner-1x1-5-area") . name /= Just "div"))) filterIng1
            let filterIng3    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "id" . only "banner-1x1-9-area") . name /= Just "div"))) filterIng2
            let filterIng4    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "id" . only "preparo") . name /= Just "div"))) filterIng3
            let filterIng5    = fmap (transform (children %~ filter (\z -> z ^? element . name /= Just "hr"))) filterIng4
            let filterIng6    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "mais-receitas mt15 mb15 grid-sm-12") . name /= Just "div"))) filterIng5
            let filterIng7    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "player_dynad_tv") . name /= Just "div"))) filterIng6
            let filterIng8    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "green txt-large font-serif txt-bold mb25 mt25") . name /= Just "h2"))) filterIng7
            let filterIng9    = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . name /= Just "ol"))) filterIng8
            let filterIng10   = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipe__actions centering grid-sm-12 grid-lg-12 white mb10 font-serif txt-very-small omega") . name /= Just "div"))) filterIng9
            let filterIng11   = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "txt-center printable--none") . name /= Just "div"))) filterIng10
            let modoDePreparo = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12")
            return $ h1 `mappend` tempPrepRend `mappend` img `mappend` filterIng11 `mappend` modoDePreparo

-- ix "class" . only "card--half-image--without-image"
-- https://cybercook.uol.com.br/resultado.php?q=arroz
-- https://cybercook.uol.com.br/resultado.php?q=caldo+de+feijão