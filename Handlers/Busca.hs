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
import Scraper.Ztestes.Boott as B
import Scraper.Busca.Receita as R
import Scraper.Busca.TudoGostoso as TG
import Text.Taggy 
import Text.Taggy.Lens 

postBuscaR :: Handler Html
postBuscaR = do
    ((res', widget), enctype) <- runFormPost form
    case res' of
        FormSuccess res -> do
            case (buscaCampo3 res) of
                Nothing -> liftIO (TG.busca "") >>= \y -> defaultLayout [whamlet|
                    #{Prelude.map (toMarkup False) y}
                |]
                Just x -> liftIO (TG.busca $ unpack x) >>= \y -> defaultLayout [whamlet|
                   #{Prelude.map (toMarkup False) y}
                |]
        _ -> redirect  HomeR
        
getOutroR :: Handler Html
getOutroR = do
    h2 <- liftIO $ TG.busca "umami"
    defaultLayout [whamlet|
         #{Prelude.map (toMarkup False) h2}
    |]