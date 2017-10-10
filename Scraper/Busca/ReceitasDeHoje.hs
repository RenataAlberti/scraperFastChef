{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Busca.ReceitasDeHoje where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Data.Tree.Lens
import Scraper.Padrao
import Yesod.Core

s x = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.allrecipes.com.br"]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ constructUrl ReceitasDeHoje x
            let fullBody = r ^. responseBody . to LE.decodeUtf8
            let lente = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "cat-view")
            let filterH1 = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "main-title") . name /= Just "h1"))) lente
            let filterCat = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "cat") . name /= Just "span"))) filterH1
            let filterRating = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "rate") . name /= Just "div"))) filterCat
            let filterDiv = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "clearall") . name /= Just "div"))) filterRating
            let filterNavigation = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "navigation") . name /= Just "div"))) filterDiv
            let filterScript = fmap (transform (children %~ filter (\z -> z ^? element . name /= Just "script"))) filterNavigation
            return filterScript

-- http://www.receitasdehoje.com.br
-- http://www.receitasdehoje.com.br/?s=bolo