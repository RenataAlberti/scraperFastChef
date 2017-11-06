{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Text

{- Tipos -}
data MyRoute = AllRecipes | CyberCook | ReceitasDeHoje deriving Show

data TypeRoute = Search | View

data Recipes = Recipes{
    nome  :: String,
    link  :: String,
    img   :: String
} deriving (Generic, Show)

data Recipe = Recipe{
    h1 :: String,
    imagem :: String,
    rendimento :: String,
    preparo :: String,
    fonte :: Fonte,
    ingredientes :: [Lista]
} deriving (Generic, Show)


data Lista = Lista{
    h3 :: Maybe String,
    ingrediente  :: [String]
} deriving (Generic, Show)

data Fonte = Fonte{
    servico :: MyRoute,
    fonteUrl :: String
} deriving (Generic, Show)

primeiro preLista lista = fst (DT.span (\x -> x /= (DT.last preLista)) lista)


segundo preList list = snd (DT.span (\x -> x /= (DT.last preList)) list)


inicio preLis = DT.init preLis



colocanomaybe :: String -> Maybe String
colocanomaybe l = (Just l)

-- 
compareLength' title preList
    | (DT.length title) /= (DT.length preList) = [""] ++ title
    | otherwise = title


--categoryOne title preList list = do
--categoryTwo title preList list = do
    


-- tres listas
categoryThree title preList list = do
    let lista = compareLength' title preList
    let a = snd (DT.span (\x -> x /= (DT.last preList)) list)
    let b = fst (DT.span (\x -> x /= (DT.last preList)) list)
    let c = DT.init preList
    let d = snd (DT.span (\x -> x /= (DT.last c)) b)
    let e = fst (DT.span (\x -> x /= (DT.last c)) b)
    let f = Lista  (Just (DT.head lista)) e
    let g = DT.tail lista
    let h = Lista (Just (DT.head  g)) d
    let i = DT.tail g
    let j = Lista (Just (DT.head  i)) a
    a <- [f, h, j]
    return a

   

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
treeMap (a:as) (b:bs) (c:cs) = Recipes (joinCharacters "<a>" a) b (splitList 1 c) :(treeMap as bs cs)
treeMap _ _ _ = []


joinCharacters :: String -> String -> String
joinCharacters x y = x ++ y



{-
category Nothing [] = Lista Nothing []
category Nothing (y:ys) = Lista Nothing y :(category ys)
category (Just x) (y:ys) = Lista (Just x) y:(category ys)
-}

-- head Lists = ["\n6 unidades de clara de ovo batidas em neve ","\n500 ml de creme de leite fresco ","\n1 x\237cara (ch\225) de a\231\250car "]
-- Maybe h3 - ["Massa","Recheio","Calda","Massa","Recheio","Calda"]
-- idRecipe x = (reverse x)


-- https://cybercook.uol.com.br/bolo-de-aniversario-r-12-108462.html
-- https://cybercook.uol.com.br/receita-de-bombom-de-morango-gigante-r-7-121441.html

{- Funções padrão CyberCook -}
linkReverse x = Prelude.reverse x


htmlRemove x y = DT.drop x y


hifenRemove x = unpack (Data.Text.takeWhile (/= '-') (pack x))

idRemove x = linkReverse (hifenRemove (htmlRemove 5 (linkReverse x))) :: [Char]


{- Instâncias 
instance FromJSON Recipe where
    parseJSON (Object o) = Recipe <$>
                           o .: "nome" <*>
                           o .: "link" <*>
                           o .: "img"  
    parseJSON _ = mzero

instance ToJSON Recipe where
    toJSON (Recipe n l i) = object ["nome" Data.Aeson..= n, "link" Data.Aeson..= l, "img" Data.Aeson..= i]
-}

