{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Scraper.Services.ReceitasIg where

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
header' = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receitasIg&rlz=1C1AVFB_enBR731BR732&oq=receitasIg&aqs=chrome..69i57j0l5.3284j0j7&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["https://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess $ "http://receitas.ig.com.br/busca/?q=bolo"
            return $ r ^.. responseBody . to LE.decodeUtf8 . html . allNamed(only "a") . attr "href" . _Just

--lente x = (x ^.. html . allNamed(only "h3") . attributed(ix "class" . only "txt-large txt-shadow") . to universe . traverse . contents)
-- [1 | _ <- xs]  
  
-- insertList xs e 0 = e:xs
-- insertList (x:xs) e n = x:(insert_list xs e (n-1))

--removeList (x:xs) e 0 = xs
--removeList (x:xs) e n = x:(remove_list xs e (n-1)) 

-- (DL.concat [(DL.take 1 link), (DL.take 1 lente), (DL.take 6 link), (DL.take 7 link)])data MyRoute = SomePage
data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "http://receitas.ig.com.br/busca/" `append`
    TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) params))
    
constructUrl x = unpack $ render SomePage [(pack "q", pack x)]