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


searchCyberCook x = do
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
    let nm      = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "list") . allNamed(only "div") . allNamed(only "h3") . children . traverse . contents
    let im      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "img") . attr "data-pagespeed-lazy-src" . _Just
    let lin     = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "content grid-lg-8") . allNamed(only "section") . attributed (ix "class" . only "grid-lg-12") . allNamed(only "div") . attributed (ix "class" . only "pr20 pl20") . allNamed(only "a") . attributed (ix "class" . only "clickable") .attr "href" . _Just
    let font   = (replicate (DT.length nm) "https://cybercook.uol.com.br")
    return $ recipeMap (fmap unpack (removeElements 8 (removeRepetition nm))) (fmap unpack lin) (fmap unpack im) (fmap unpack font) :: IO [Recipes]


-- T.putStrLn . T.decodeUtf8 . encode $ Recipe "Guacamole" "linkkahshss" "imggjsjdjsd"



    --  - ok
    -- recipeMap (fmap unpack nm) (fmap unpack lin) (fmap unpack im) (fmap unpack font) :: IO [Recipes]



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


detalhe' :: String -> IO Recipe
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
            let titulo = fullBody ^.. html . allNamed(only "h1") . contents
            let im      = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "swiper-content pos-relative") . allNamed(only "img") . attributed(ix "class" . only "photo") . attr "src" . _Just
            let list  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only (pack (joinCharacters "printable-" (idRemove view)))) . allNamed(only "ul") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . allNamed(only "li") . children . ix 1 . contents
            let preList  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only (pack (joinCharacters "printable-" (idRemove view)))) . allNamed(only "ul") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . children . ix 0 .  allNamed(only "label") . contents
            let preMdp = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . children . ix 0 . allNamed(only "div") . attributed(ix "class" . only "grid-lg-11 grid-sm-11 omega") . contents
            let mdp = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . allNamed(only "li") . element . children . ix 1 . contents
            let h3 = fullBody ^.. html . allNamed(only "h3") . attributed(ix "class" . only "font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10") . contents
            let receita = (Recipe (unpack (DT.head titulo)) (unpack (DT.head im)) (Fonte CyberCook view) (comparePreList (DT.map unpack h3) (DT.map unpack preList) (DT.map unpack list)) (comparePreList (DT.map unpack h3) (DT.map unpack preMdp) (DT.map unpack mdp)))
            return $ receita :: IO Recipe
            

filterLens x = fmap (transform
                        (children %~ 
                            Prelude.filter (\z -> z ^? element . attributed(ix "class" . only "grid-lg-1 grid-sm-1")
                            . name /= Just "input")
                        )
                    ) x 
                    
recebe x = x . element . children . traverse . element . children


-- receita-de-empanada-da-paola-carosella-r-13-121379.html
-- treeMap (fmap unpack nm) (fmap unpack lin) (fmap unpack im) :: IO [Recipes]
-- ix "class" . only "card--half-image--without-image"
-- https://cybercook.uol.com.br/resultado.php?q=arroz
-- https://cybercook.uol.com.br/resultado.php?q=caldo+de+feijão