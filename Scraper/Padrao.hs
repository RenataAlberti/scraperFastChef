{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Padrao where

import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Data.Text (Text, append, pack, unpack)
import qualified Data.List as DL
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)

{-
data Receita a = ReceitaDesc{nomeReceita  :: Text,
                           linkReceita  :: Text,
                           imgReceita   :: Text,
                           fonteReceita :: Text,
                           prepReceita  :: Text} deriving Show
-}

data Receita = ReceitaDesc{nomeReceita  :: Text, linkReceita  :: Text, imgReceita   :: Text, fonteReceita :: Text, prepReceita  :: Text} deriving Show

-- instance (Read a) => 

data MyRoute = AllRecipes | CyberCook | ReceitasDeHoje | FastChef

renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl AllRecipes q = "http://allrecipes.com.br/receitas/resultados-de-busca.aspx" 
                      `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) q))
renderUrl CyberCook q = "https://cybercook.uol.com.br/resultado.php"
                     `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) q))
renderUrl ReceitasDeHoje q = "http://www.receitasdehoje.com.br/"
                          `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DL.map (second Just) q))
    
constructUrl AllRecipes x = unpack $ renderUrl AllRecipes [(pack "texto", pack x)]
constructUrl CyberCook x = unpack $ renderUrl CyberCook [(pack "q", pack x)]
constructUrl ReceitasDeHoje x = unpack $ renderUrl ReceitasDeHoje [(pack "s", pack x)]

