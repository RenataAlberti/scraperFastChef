{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper.General where

import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE
import qualified Data.List as DT
import Control.Arrow (second)
import Blaze.ByteString.Builder (toByteString)
import GHC.Generics
import Data.Text

{- Tipos -}

data Site = AllRecipes | CyberCook | ReceitasDeHoje deriving (Show, Read)

data TypeRoute = Search | View

data Recipes = Recipes{
    titulo  :: String,
    lin  :: String,
    img   :: String,
    lincopy :: Fonte
} deriving (Generic, Show)

data Recipe = Recipe{
    h1 :: String,
    imagem :: String,
    copyright :: Fonte,
    ingredientes :: [Lista],
    modopreparo :: [Lista]
} deriving (Generic, Show, Read)

data Lista = Lista{
    h3 :: Maybe String,
    lista  :: [String]
} deriving (Generic, Show, Read)

data Fonte = Fonte{
    nm :: Site,
    fonteurl :: String
} deriving (Generic, Show, Read)


{- Funções de Url -}

renderUrl :: Site -> [(Text, Text)] -> Text
renderUrl AllRecipes q      = "http://allrecipes.com.br/receitas/resultados-de-busca.aspx" 
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True 
                                    (DT.map (second Just) q))
renderUrl CyberCook q       = "https://cybercook.uol.com.br/resultado.php"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True 
                                    (DT.map (second Just) q))
renderUrl ReceitasDeHoje q  = "http://www.receitasdehoje.com.br/"
                                    `append` TE.decodeUtf8 (toByteString $ renderQueryText True 
                                    (DT.map (second Just) q))

renderUrl' :: Site -> Text -> Text
renderUrl' AllRecipes b     = pack $ "http://allrecipes.com.br/receita/" ++ (unpack b) 
renderUrl' CyberCook b       = pack $ "https://cybercook.uol.com.br" ++ (unpack b)
renderUrl' ReceitasDeHoje b  = pack $ "http://www.receitasdehoje.com.br" ++ (unpack b)

constructUrl :: Site -> TypeRoute -> String -> String    
constructUrl AllRecipes Search x      = (unpack $ renderUrl AllRecipes [(pack "texto", pack x)]) ++ (unpack "&o_is=Search")
constructUrl CyberCook Search x       = unpack $ renderUrl CyberCook [(pack "q", pack x)]
constructUrl ReceitasDeHoje Search x  = unpack $ renderUrl ReceitasDeHoje [(pack "s", pack x)]
constructUrl AllRecipes View x        = unpack $ renderUrl' AllRecipes (pack x)
constructUrl CyberCook View x         = unpack $ renderUrl' CyberCook (pack x)
constructUrl ReceitasDeHoje View x    = unpack $ renderUrl' ReceitasDeHoje (pack x)

-- http://allrecipes.com.br/receitas/resultados-de-busca.aspx?texto=maracuj%C3%A1&o_is=Search


-- Separa em duas listas conforme o Int e retorna somente a segunda
splitList :: Int -> [a] -> [a]
splitList x a = snd (DT.splitAt x a)

-- Junta caracteres
joinCharacters :: String -> String -> String
joinCharacters x y = x ++ y

{- Funções de remoção de elementos -}
-- Remove Int elementos da lista
htmlRemove :: Int -> [a] -> [a]
htmlRemove x y = DT.drop x y

-- Remove remove os elementos até o hífen
hifenRemove :: String -> String
hifenRemove x = unpack (Data.Text.takeWhile (/= '-') (pack x))

-- Captura os 5 últimos elementos da lista, por exemplo, a extensão .html de um link
idRemove :: [Char] -> [Char]
idRemove x = Prelude.reverse (hifenRemove (htmlRemove 5 (Prelude.reverse x))) :: [Char]

-- Remove elementos repetidos na sequência
removeRepetition :: [a] -> [a]
removeRepetition a = DT.map snd $ DT.filter (odd . fst) (DT.zip [0 .. ] a)

-- Remove os elementos do final da lista
removeElements :: Int -> [a] -> [a]
removeElements x y = DT.reverse (DT.drop x (DT.reverse y))

{- Função para o tipo Recipes -}
recipeMap :: [String] -> [String] -> [String] -> [String] -> [Recipes]
recipeMap [] [] [] [] = []
recipeMap (a:as) (b:bs) (c:cs) (d:ds) = Recipes a b c (Fonte CyberCook d) :(recipeMap as bs cs ds)
recipeMap _ _ _ _ = []

{- Funções para o tipo Lista -}
comparePreList :: [String] -> [String] -> [String] -> [Lista]
comparePreList title preList list
    | (DT.length preList) < 2  = [Lista Nothing list]
    | (DT.length preList) == 2 = categoryTwo (DT.drop (DT.length title `div` 2) title) preList list
    | (DT.length preList) == 3 = categoryThree (DT.drop (DT.length title `div` 2) title) preList list
    | (DT.length preList) == 4 = categoryFour (DT.drop (DT.length title `div` 2) title) preList list
    | (DT.length preList) == 5 = categoryFive (DT.drop (DT.length title `div` 2) title) preList list
    | otherwise = [Lista Nothing list]

compareLength' :: [String] -> [String] -> [String]
compareLength' title preList
    | (DT.length title) /= (DT.length preList) = [""] ++ title
    | otherwise = title

categoryTwo :: [String] -> [String] -> [String] -> [Lista]    
categoryTwo title preList list = do
    let lista = compareLength' title preList
    let a = fst (DT.span (\x -> x /= (DT.last preList)) list)
    let b = snd (DT.span (\x -> x /= (DT.last preList)) list)
    let a' = Lista (Just (DT.head lista)) a
    let b' = Lista (Just (DT.last lista)) b
    c <- [a', b']
    return c

categoryThree :: [String] -> [String] -> [String] -> [Lista]
categoryThree title preList list = do
    let lista = compareLength' title preList
    let a = fst (DT.span (\x -> x /= (DT.last preList)) list)
    let b = snd (DT.span (\x -> x /= (DT.last preList)) list)
    let c = DT.init preList
    let d = fst (DT.span (\x -> x /= (DT.last c)) a)
    let e = snd (DT.span (\x -> x /= (DT.last c)) a)
    let f = Lista  (Just (DT.head lista)) d
    let g = DT.tail lista
    let h = Lista (Just (DT.head  g)) e
    let i = DT.tail g
    let j = Lista (Just (DT.head  i)) b
    a' <- [f, h, j]
    return a'

categoryFour :: [String] -> [String] -> [String] -> [Lista]
categoryFour title preList list = do
    let lista = compareLength' title preList
    let a = fst (DT.span (\x -> x /= (DT.last preList)) list)
    let b = snd (DT.span (\x -> x /= (DT.last preList)) list)
    let c = DT.init preList
    let d = fst (DT.span (\x -> x /= (DT.last c)) a)
    let e = snd (DT.span (\x -> x /= (DT.last c)) a)
    let f = DT.init c
    let g = fst (DT.span (\x -> x /= (DT.last f)) d)
    let h = snd (DT.span (\x -> x /= (DT.last f)) d)
    let i = Lista  (Just (DT.head lista)) g
    let j = DT.tail lista
    let k = Lista (Just (DT.head  j)) h
    let l = DT.tail j
    let m = Lista (Just (DT.head  l)) e
    let n = DT.tail l
    let o = Lista (Just (DT.head n)) b
    a' <- [i, k, m, o]
    return a'

categoryFive :: [String] -> [String] -> [String] -> [Lista]
categoryFive title preList list = do
    let lista = compareLength' title preList
    let a = fst (DT.span (\x -> x /= (DT.last preList)) list)
    let b = snd (DT.span (\x -> x /= (DT.last preList)) list)
    let c = DT.init preList
    let d = fst (DT.span (\x -> x /= (DT.last c)) a)
    let e = snd (DT.span (\x -> x /= (DT.last c)) a)
    let f = DT.init c
    let g = fst (DT.span (\x -> x /= (DT.last f)) d)
    let h = snd (DT.span (\x -> x /= (DT.last f)) d)
    let i = DT.init f
    let j = fst (DT.span (\x -> x /= (DT.last i)) g)
    let k = snd (DT.span (\x -> x /= (DT.last i)) g)
    let l = Lista  (Just (DT.head lista)) j
    let m = DT.tail lista
    let n = Lista (Just (DT.head  m)) k
    let o = DT.tail m
    let p = Lista (Just (DT.head  o)) h
    let q = DT.tail o
    let r = Lista (Just (DT.head q)) e
    let s = Lista (Just (DT.last q)) b
    a' <- [l, n, p, r, s]
    return a'
    
    
    
    