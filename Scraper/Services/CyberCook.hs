{-# LANGUAGE OverloadedStrings #-}

module Scraper.Services.CyberCook where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens 
import Control.Lens hiding (children, element)
import Scraper.General
import qualified Data.List as DT
import Data.Text (pack, unpack)
import qualified Data.ByteString.Char8 as DBC


searchCyberCook :: String -> IO [Recipes]
searchCyberCook x = do
    let search = constructUrl CyberCook Search x
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 ..."]
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


viewCyberCook :: String -> IO Recipe
viewCyberCook x = do
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