{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Padrao where

import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Data.Text (Text, append, pack, unpack)
import qualified Data.List as DL
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)


data MyRoute = AllRecipes | CyberCook

render :: MyRoute -> [(Text, Text)] -> Text
render AllRecipes q = "http://allrecipes.com.br/receitas/resultados-de-busca.aspx" `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) q))
render CyberCook q = "https://cybercook.uol.com.br/resultado.php" `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) q))

    
constructUrl AllRecipes x = unpack $ render AllRecipes [(pack "texto", pack x)]
constructUrl CyberCook x = unpack $ render CyberCook [(pack "q", pack x)]
