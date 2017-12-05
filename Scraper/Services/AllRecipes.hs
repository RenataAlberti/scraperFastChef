{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Services.AllRecipes where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens 
import Control.Lens hiding (children, element)
import Scraper.General
import qualified Data.List as DT
import Data.Text
import qualified Data.ByteString.Char8 as DBC
import Yesod.Core

searchAllRecipes :: String -> IO [Recipes]
searchAllRecipes x = do
    let search = constructUrl AllRecipes Search x
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36"]
                   & header "Accept" .~ ["text/html, */*"]
                   & header "X-Requested-With" .~ ["XMLHttpRequest"]
                   & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                   & header "Accept-Encoding" .~ ["gzip, deflate"]
                   & header "Referer" .~ [DBC.pack search]
                   & header "Origin" .~ ["http://allrecipes.com.br"]
                   & header "Connection" .~ ["keep-alive"]
    r <- S.withSession $ \sess -> do
        S.getWith header' sess $ search 
    let fullBody     = r ^. responseBody . Control.Lens.to LE.decodeUtf8
    let nome         = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "sectionTopRecipes gridResponsive__module") . allNamed(only "div") . attributed(ix "class" . only "row recipe") . allNamed(only "a") . attributed(ix "itemprop" . only "name") . contents
    let linkoriginal = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "sectionTopRecipes gridResponsive__module") . allNamed(only "div") . attributed(ix "class" . only "row recipe") . allNamed(only "a") . attributed(ix "itemprop" . only "name") . attr "href" . _Just
    let img          = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "sectionTopRecipes gridResponsive__module") . allNamed(only "div") . attributed(ix "class" . only "row recipe") . allNamed(only "img") . attr "src" . _Just
    let fonte         = (Prelude.replicate (DT.length nome) "http://allrecipes.com.br")
    return $ recipeMap' (fmap unpack nome) (fmap (DT.drop 33) (DT.map unpack linkoriginal)) (fmap unpack img) (fmap unpack fonte) :: IO [Recipes]
    
--(fmap unpack (removeElements 8 (removeRepetition nm))) 
viewAllRecipes :: String -> IO Recipe
viewAllRecipes x = do
            let view = constructUrl AllRecipes View x
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ [DBC.pack view]
                           & header "Origin" .~ ["http://www.allrecipes.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ view
            let fullBody      = r ^. responseBody . Control.Lens.to LE.decodeUtf8
            let titulo = fullBody ^.. html . allNamed(only "span") . attributed(ix "itemprop" . only "name") . contents
            let im      = fullBody ^.. html . allNamed(only "span") . allNamed(only "img") . attributed(ix "class" . only "recipe-img  largeImg") . attr "src" . _Just
            let mdp = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeDirections gridResponsive__module") . allNamed(only "ol") . allNamed(only "li") . children . ix 0 . contents
            let ing = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeIngredients gridResponsive__module") . allNamed(only "ul") . allNamed(only "li") . allNamed(only "span") . contents
            let h3 = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeIngredients gridResponsive__module") . allNamed(only "ul") . allNamed(only "li") . allNamed(only "b") . contents
            let receita = (Recipe (unpack (DT.head titulo)) (unpack (Data.Text.replace "large" "port500" (DT.head im))) (Fonte AllRecipes view) (comparePreList (DT.map unpack [""]) (DT.map unpack [""]) (DT.map unpack ing)) (comparePreList (DT.map unpack [""]) (DT.map unpack [""]) (DT.map unpack mdp)))
            return $ receita :: IO Recipe
 