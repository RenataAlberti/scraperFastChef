{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Padrao where

import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Data.Text (Text, append, pack, unpack)
import qualified Data.List as DT
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)

{- Tipos -}
data MyRoute = AllRecipes | CyberCook | ReceitasDeHoje

data TypeRoute = Search | View

data Recipe = Recipe{
    nome :: String,
    link :: String,
    img :: String
} deriving (Show)


{- Funções padrão -}

renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl AllRecipes q      = "http://allrecipes.com.br/receitas/resultados-de-busca.aspx" 
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))
renderUrl CyberCook q       = "https://cybercook.uol.com.br/resultado.php"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))
renderUrl ReceitasDeHoje q  = "http://www.receitasdehoje.com.br/"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))


-- Recebe o site + o termo da busca e constrói a url com base no padrao de cada site.
constructUrl :: MyRoute -> TypeRoute -> String -> String    
constructUrl AllRecipes Search x      = unpack $ renderUrl AllRecipes [(pack "texto", pack x)]
constructUrl CyberCook Search x       = unpack $ renderUrl CyberCook [(pack "q", pack x)]
constructUrl ReceitasDeHoje Search x  = unpack $ renderUrl ReceitasDeHoje [(pack "s", pack x)]
constructUrl AllRecipes View x        = unpack $ renderUrl AllRecipes [(pack "", pack x)]
constructUrl CyberCook View x         = unpack $ renderUrl AllRecipes [(pack "", pack x)]
constructUrl ReceitasDeHoje View x    = unpack $ renderUrl AllRecipes [(pack "", pack x)]

--constructUrlDirect AllRecipes x = unpack $ 


splitList :: Int -> [a] -> [a]
splitList x a = snd (DT.splitAt x a)


{-class IsString MyRoute where
    fromString "NotFound" = NotFound
    fromString "AlRecipes" = AllRecipes
    fromString "CyberCook" = CyberCook
    fromString "ReceitasDeHoje" = ReceitasDeHoje
    fromString "FastChef" = FastChef
  -}  
--class MyRoute where
--  foo :: a -> String

--instance MyRoute String where
--  foo _ = "String"

--instance Foo Text where
--  foo _ = "Text"