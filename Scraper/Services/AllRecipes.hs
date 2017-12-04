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
import Data.Text (pack, unpack)
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
    let fonte         = (replicate (DT.length nome) "http://allrecipes.com.br")
    return $ recipeMap' (fmap unpack nome) (fmap unpack linkoriginal) (fmap unpack img) (fmap unpack fonte) :: IO [Recipes]