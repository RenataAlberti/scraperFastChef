{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handlers.Busca where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text(Text, pack, unpack)
import Yesod.Form
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Utils.WidgetResultadoBusca
import Utils.SettingsForm
import Scraper.Ztestes.Boott
import Scraper.Busca.Receita as R
import Text.Taggy 
import Text.Taggy.Lens 

postBuscaR :: Handler Html
postBuscaR = do
    ((res', widget), enctype) <- runFormPost form
    case res' of
        FormSuccess res -> do
            case (buscaCampo3 res) of
                Nothing -> liftIO (R.header' "teste") >>= \y -> defaultLayout [whamlet|
                      
                      ^{widgetResultNothing y}
                |]
                Just x -> liftIO (R.header' $ unpack x) >>= \y -> defaultLayout [whamlet|
                
                    ^{widgetResultJust y}
                |]
        _ -> redirect  HomeR
        
getOutroR :: Handler Html
getOutroR = do
    h2 <- liftIO $ header2 "teste"
    defaultLayout [whamlet|
        #{Prelude.map (toMarkup False) h2}
    |]