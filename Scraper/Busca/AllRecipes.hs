{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Busca.AllRecipes where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Scraper.Padrao


texto x = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.allrecipes.com.br"]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess $ constructUrl AllRecipes x
            let fullBody = r ^. responseBody . to LE.decodeUtf8
            let lente = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "row recipe")
            let filterDesc = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "js-truncate") . name /= Just "p"))) lente
            let filterReviewCount = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipeReviewCount") . name /= Just "div"))) filterDesc
            return filterReviewCount


--http://allrecipes.com.br/receitas/resultados-de-busca.aspx?texto=morango%20a%C3%A7%C3%BAcar
--http://www.tudogostoso.com.br/busca.php?q=morango+a%E7%FAcar
-- div class row recipe