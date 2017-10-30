{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Busca.AllRecipes where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Data.Tree.Lens
import Scraper.Padrao
import Yesod.Core

{-
texto x = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.allrecipes.com.br"]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ constructUrl AllRecipes x
            let fullBody          = r ^. responseBody . to LE.decodeUtf8
            let lente             = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "row recipe")
            let nomedareceita     = fullBody ^.. html . allNamed (only "h1") . children . traverse . content 
            let filterDiv         = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "iconHolder") . name /= Just "div"))) lente
            let filterSpan        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "playButton") . name /= Just "span"))) filterDiv
            let filterDesc        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "js-truncate") . name /= Just "p"))) filterSpan
            let filterReviewCount = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipeReviewCount") . name /= Just "div"))) filterDesc
            let as = show nomedareceita
            return $ nomedareceita
  -}          

texto x = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.allrecipes.com.br"]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ constructUrl AllRecipes x
            let fullBody          = r ^. responseBody . to LE.decodeUtf8
            let lente             = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "row recipe")
            let filterDiv         = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "iconHolder") . name /= Just "div"))) lente
            let filterSpan        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "playButton") . name /= Just "span"))) filterDiv
            let filterDesc        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "js-truncate") . name /= Just "p"))) filterSpan
            let filterReviewCount = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipeReviewCount") . name /= Just "div"))) filterDesc
            return filterReviewCount


detalhe = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.allrecipes.com.br"]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess "http://allrecipes.com.br/receita/7449/torta--de-frutas-com-creme.aspx"
            let fullBody      = r ^. responseBody . to LE.decodeUtf8
            let h1            = fullBody ^.. html . allNamed(only "h1")
            let tempPrepRend  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "stat1")
            let img           = fullBody ^.. html . allNamed(only "img") . attributed(ix "class" . only "recipe-img  largeImg")
            let ingredientes  = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeIngredients gridResponsive__module")
            let modoDePreparo = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeDirections gridResponsive__module")
            let filterModoDep = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "epsilon") . name /= Just "small"))) modoDePreparo
            return $ h1 `mappend` tempPrepRend `mappend` img `mappend` ingredientes `mappend` filterModoDep
            
--http://allrecipes.com.br/receitas/resultados-de-busca.aspx?texto=morango%20a%C3%A7%C3%BAcar
--http://www.tudogostoso.com.br/busca.php?q=morango+a%E7%FAcar
-- div class row recipe