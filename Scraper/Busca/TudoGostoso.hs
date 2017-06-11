{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Scraper.Busca.TudoGostoso where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Network.Wreq
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children)
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import qualified Data.List as DL
import Data.Text (Text, append, pack, unpack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Blaze.ByteString.Builder (toByteString)
import qualified Text.Taggy.Renderer as Renderer

data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "http://www.tudogostoso.com.br/busca.php" `append`
    TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) params))
    
constructUrl x = unpack $ render SomePage [(pack "q", pack x)]

constructDirectUrl x = "http://www.tudogostoso.com.br/receita" ++ x

busca x = do
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
            let lente = (fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "listing box") . allNamed(only "ul") . attributed(ix "class" . only "clearfix"))
            let filterRecipes = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "gright right-list") . name /= Just "div"))) lente
            let filterAuthor = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "author") . name /= Just "span"))) filterRecipes
            let filterRating = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "rating") . name /= Just "span"))) filterAuthor
            let filterLikes = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipe-info like-and-tags") . name /= Just "div"))) filterRating
            let filterPagination = fmap (transform (children %~ filter (\n -> n ^? element . attributed(ix "class" . only "pagination") . name /= Just "div"))) filterLikes
            return $ filterPagination