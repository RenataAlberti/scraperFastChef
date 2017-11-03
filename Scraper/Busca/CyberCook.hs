{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.List as DT
import Data.Text (pack, unpack)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Aeson as Aeson


-- T.putStrLn . T.decodeUtf8 . encode $ Recipe "Guacamole" "linkkahshss" "imggjsjdjsd"

haha x = do
    let search = constructUrl CyberCook Search x
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                   & header "Accept" .~ ["text/html, */*"]
                   & header "X-Requested-With" .~ ["XMLHttpRequest"]
                   & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                   & header "Accept-Encoding" .~ ["gzip, deflate"]
                   & header "Referer" .~ [DBC.pack search]
                   & header "Origin" .~ ["https://cybercook.uol.com.br"]
                   & header "Connection" .~ ["keep-alive"]
    r <- S.withSession $ \sess -> do
        S.getWith header' sess $ search
    let fullBody     = r ^. responseBody . Control.Lens.to LE.decodeUtf8
    let nm      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "h3") . children . traverse . contents
    let im      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "img") . attr "data-pagespeed-lazy-src" . _Just
    let lin     = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "a") . attributed (ix "class" . only "clickable") .attr "href" . _Just
    
    return $ treeMap (fmap unpack nm) (fmap unpack lin) (fmap unpack im) :: IO [Recipes]

hoho x = do
    let view = constructUrl CyberCook View x
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                   & header "Accept" .~ ["text/html, */*"]
                   & header "X-Requested-With" .~ ["XMLHttpRequest"]
                   & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                   & header "Accept-Encoding" .~ ["gzip, deflate"]
                   & header "Referer" .~ ["https://www.google.com.br/search?rlz=1C1AVFA_enBR749BR752&ei=VXH6WeuyLcHimAG8oZTQDQ&q=cybercook+receitas&oq=cybercook+receitas&gs_l=psy-ab.3..0l3j0i22i30k1l7.8043.9363.0.10797.9.7.0.0.0.0.568.568.5-1.1.0....0...1.1.64.psy-ab..8.1.567....0.4uTXplKOXXY"]
                   & header "Origin" .~ ["https://www.google.com.br"]
                   & header "Connection" .~ ["keep-alive"]
    r <- S.withSession $ \sess -> do
        S.getWith header' sess $ view
    let fullBody     = r ^. responseBody . Control.Lens.to LE.decodeUtf8
    let nm      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "h3") . children . traverse . contents
    let im      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "img") . attr "data-pagespeed-lazy-src" . _Just
    let lin     = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "a") . attributed (ix "class" . only "clickable") .attr "href" . _Just
    
    let ing = fullBody ^.. html . allNamed(only "ul") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . allNamed(only "li") . children . traverse . contents
    let mdp = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . allNamed(only "li") . children . traverse . contents
    return $ treeMap (fmap unpack nm) (fmap unpack lin) (fmap unpack im) :: IO [Recipes]

{-ingredient-list grid-lg-12 grid-sm-12

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
        S.getWith header' sess $ constructUrl CyberCook Search x
    let fullBody     = r ^. responseBody . Control.Lens.to LE.decodeUtf8
    let lenteDiv     = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "list")
    let filterDiv    = fmap (transform (children %~ Prelude.filter (\z -> z ^? element . attributed(ix "class" . only "grid-lg-9") . name /= Just "div"))) lenteDiv
    let filterImg    = fmap (transform (children %~ Prelude.filter (\z -> z ^? element . attributed(ix "class" . only "card__flag") . name /= Just "div"))) filterDiv
    let filterRating = fmap (transform (children %~ Prelude.filter (\z -> z ^? element . attributed(ix "class" . only "card__score txt-small") . name /= Just "div"))) filterImg
    return filterRating

detalhe' x = do
            let view = constructUrl CyberCook View x
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receita+de+bolo&rlz=1C1AVFA_enBR749BR752&ei=uOP5WcXpBMKSwgSooiY&start=20&sa=N&biw=1366&bih=662"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ view
            let fullBody      = r ^. responseBody . Control.Lens.to LE.decodeUtf8
            let h1            = fullBody ^.. html . allNamed(only "h1")
            let tempPrepRend  = fullBody ^.. html . allNamed(only "p") . attributed(ix "class" . only "font-serif pb20")
            let img           = fullBody ^.. html . allNamed(only "img") . attributed(ix "class" . only "photo")
            let ingredientes  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only (pack (joinCharacters "printable-" (idRemove view))))
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
            let h3s      = fullBody ^.. html . allNamed (only "h3") . attributed (ix "class" . only "font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10")
            let modoDePreparo = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12")
            -- font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10
            return $ h1 `mappend` tempPrepRend `mappend` img `mappend` filterIng11 `mappend` h3s `mappend` modoDePreparo


detalheTest x = do
            let view = constructUrl CyberCook View x
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receita+de+bolo&rlz=1C1AVFA_enBR749BR752&ei=uOP5WcXpBMKSwgSooiY&start=20&sa=N&biw=1366&bih=662"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ view
            let fullBody      = r ^. responseBody . Control.Lens.to LE.decodeUtf8
            let h1            = fullBody ^.. html . allNamed(only "h1")
            let tempPrepRend  = fullBody ^.. html . allNamed(only "p") . attributed(ix "class" . only "font-serif pb20")
            let img           = fullBody ^.. html . allNamed(only "img") . attributed(ix "class" . only "photo")
            let ingredientes  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only (pack (joinCharacters "printable-" (idRemove view))))
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
            let h3s      = fullBody ^.. html . allNamed (only "h3") . attributed (ix "class" . only "font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10")
            let modoDePreparo = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12")
            -- font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10
            return $ h1 `mappend` tempPrepRend `mappend` img `mappend` filterIng11 `mappend` h3s `mappend` modoDePreparo

-- ix "class" . only "card--half-image--without-image"
-- https://cybercook.uol.com.br/resultado.php?q=arroz
-- https://cybercook.uol.com.br/resultado.php?q=caldo+de+feijão