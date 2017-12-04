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
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 ..."]
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
    return $ recipeMap' (fmap unpack nome) (fmap unpack linkoriginal) (fmap unpack img) (fmap unpack fonte) :: IO [Recipes]
    

viewAllRecipes x = do
            let view = constructUrl AllRecipes View x
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receita+de+bolo&rlz=1C1AVFA_enBR749BR752&ei=uOP5WcXpBMKSwgSooiY&start=20&sa=N&biw=1366&bih=662"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess $ view
            let fullBody      = r ^. responseBody . Control.Lens.to LE.decodeUtf8
            let titulo = fullBody ^.. html . allNamed(only "span") . attributed(ix "itemprop" . only "name") . contents
            let im      = fullBody ^.. html . allNamed(only "span") . allNamed(only "img") . attributed(ix "class" . only "recipe-img  largeImg") . attr "src" . _Just
            let list  = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeIngredients gridResponsive__module") . allNamed(only "ul") . allNamed(only "li") . children
            let preList  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only (pack (joinCharacters "printable-" (idRemove view)))) . allNamed(only "ul") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . children . ix 0 .  allNamed(only "label") . contents
            let preMdp = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . children . ix 0 . allNamed(only "div") . attributed(ix "class" . only "grid-lg-11 grid-sm-11 omega") . contents
            let mdp = fullBody ^.. html . allNamed(only "ol") . attributed(ix "class" . only "ingredient-list grid-lg-12 grid-sm-12") . allNamed(only "li") . element . children . ix 1 . contents
            let h3 = fullBody ^.. html . allNamed(only "h3") . attributed(ix "class" . only "font-serif txt-bold mb10 grid-lg-12 grid-sm-12 mt10") . contents
            -- let receita = (Recipe (unpack (DT.head titulo)) (unpack (Data.Text.replace "large" "port500" (DT.head im))) (Fonte AllRecipes view) (comparePreList (DT.map unpack h3) (DT.map unpack preList) (DT.map unpack list)) (comparePreList (DT.map unpack h3) (DT.map unpack preMdp) (DT.map unpack mdp)))
            -- return $ receita :: IO Recipe
            return list
            
  {-
  let receita = 
            (Recipe (unpack (DT.head titulo)) 
            (unpack (DT.head im)) 
            (Fonte CyberCook view) 
            (comparePreList (DT.map unpack h3) 
            (DT.map unpack preList) 
            (DT.map unpack list)) 
            (comparePreList (DT.map unpack h3) 
            (DT.map unpack preMdp) 
            (DT.map unpack mdp)))
            return $ receita :: IO Recipe    
  -}  
    
    
{-
texto x = do
    let search = constructUrl AllRecipes Search x
    let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                   & header "Accept" .~ ["text/html, */*"]
                   & header "X-Requested-With" .~ ["XMLHttpRequest"]
                   & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                   & header "Accept-Encoding" .~ ["gzip, deflate"]
                   & header "Referer" .~ [DBC.pack search]
                   & header "Origin" .~ ["http://www.allrecipes.com.br"]
                   & header "Connection" .~ ["keep-alive"]
    r <- S.withSession $ \sess -> do
        S.getWith header' sess $ search
    let fullBody          = r ^. responseBody . to LE.decodeUtf8
    let lente             = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "row recipe")
    let filterDiv         = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "iconHolder") . name /= Just "div"))) lente
    let filterSpan        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "playButton") . name /= Just "span"))) filterDiv
    let filterDesc        = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "js-truncate") . name /= Just "p"))) filterSpan
    let filterReviewCount = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "recipeReviewCount") . name /= Just "div"))) filterDesc
    return filterReviewCount


detalhe = do
            let header' = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=receitas+allrecipes&rlz=1C1AVFA_enBR749BR752&oq=receitas+allrecipes&aqs=chrome..69i57j69i60l3j69i65j0.14064j0j7&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["https://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith header' sess "http://allrecipes.com.br/receita/6804/batata-gratinada-com-bacon.aspx"
            let fullBody      = r ^. responseBody . to LE.decodeUtf8
            let h1            = fullBody ^.. html . allNamed(only "h1")
            let tempPrepRend  = fullBody ^.. html . allNamed(only "div") . attributed(ix "class" . only "stat1")
            let img           = fullBody ^.. html . allNamed(only "img") . attributed(ix "class" . only "recipe-img  largeImg")
            let ingredientes  = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeIngredients gridResponsive__module")
            let modoDePreparo = fullBody ^.. html . allNamed(only "section") . attributed(ix "class" . only "recipeDirections gridResponsive__module")
            let filterModoDep = fmap (transform (children %~ filter (\z -> z ^? element . attributed(ix "class" . only "epsilon") . name /= Just "small"))) modoDePreparo
            return $ h1 `mappend` tempPrepRend `mappend` img `mappend` ingredientes `mappend` filterModoDep

-}
   
--http://allrecipes.com.br/receitas/resultados-de-busca.aspx?texto=morango%20a%C3%A7%C3%BAcar
--http://www.tudogostoso.com.br/busca.php?q=morango+a%E7%FAcar
-- div class row recipe