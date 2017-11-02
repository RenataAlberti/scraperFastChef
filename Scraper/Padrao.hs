{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveAnyClass #-}

module Scraper.Padrao where

import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import Data.Text (Text, append, pack, unpack)
import qualified Data.List as DT
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad
import GHC.Generics

{- Tipos -}
data MyRoute = AllRecipes | CyberCook | ReceitasDeHoje

data TypeRoute = Search | View

data Recipe = Recipe{
    titulo          :: String,
    linkReceita     :: String,
    linkImagem      :: String,
    rendimento      :: String,
    tempoDePreparo  :: String,
    linkFonte       :: String,
    ingredientes    :: Maybe String,
    modoDePreparo   :: Maybe String
} deriving (Generic, Show)


{- Funções padrão -}
renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl AllRecipes q      = "http://allrecipes.com.br/receitas/resultados-de-busca.aspx" 
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))
renderUrl CyberCook q       = "https://cybercook.uol.com.br/resultado.php"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))
renderUrl ReceitasDeHoje q  = "http://www.receitasdehoje.com.br/"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True (DT.map (second Just) q))


renderUrl' :: MyRoute -> (Text, Text) -> Text
renderUrl' AllRecipes b     = pack $ "http://allrecipes.com.br/receita/" ++ (unpack (snd b))
renderUrl' CyberCook b       = pack $ "https://cybercook.uol.com.br/" ++ (unpack (snd b))
renderUrl' ReceitasDeHoje b  = pack $ "http://www.receitasdehoje.com.br/" ++ (unpack (snd b))


constructUrl :: MyRoute -> TypeRoute -> String -> String    
constructUrl AllRecipes Search x      = unpack $ renderUrl AllRecipes [(pack "texto", pack x)]
constructUrl CyberCook Search x       = unpack $ renderUrl CyberCook [(pack "q", pack x)]
constructUrl ReceitasDeHoje Search x  = unpack $ renderUrl ReceitasDeHoje [(pack "s", pack x)]
constructUrl AllRecipes View x        = unpack $ renderUrl' AllRecipes (pack "", pack x)
constructUrl CyberCook View x         = unpack $ renderUrl' CyberCook (pack "", pack x)
constructUrl ReceitasDeHoje View x    = unpack $ renderUrl' ReceitasDeHoje (pack "", pack x)


splitList :: Int -> [a] -> [a]
splitList x a = snd (DT.splitAt x a)


treeMap [] [] [] = []
treeMap (a:as) (b:bs) (c:cs) = Recipe (juntaCaracteres "<a>" a) b (splitList 1 c) :(treeMap as bs cs)
treeMap _ _ _ = []



juntaCaracteres :: String -> String -> String
juntaCaracteres x y = x ++ y



{- Instâncias -}
instance FromJSON Recipe where
    parseJSON (Object o) = Recipe <$>
                           o .: "nome" <*>
                           o .: "link" <*>
                           o .: "img"
    parseJSON _ = mzero

instance ToJSON Recipe where
    toJSON (Recipe n l i) = object ["nome" Data.Aeson..= n, "link" Data.Aeson..= l, "img" Data.Aeson..= i]
    
 