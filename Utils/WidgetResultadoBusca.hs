{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Utils.WidgetResultadoBusca where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text
import Yesod.Form
import Utils.SettingsForm
import Scraper.Services.Receita

widgetResultNothing ::[Text] -> Widget
widgetResultNothing y = do
    toWidget [hamlet|
       <p> Teste da widget #{show y}
    |]
    
widgetResultJust :: [Text] -> Widget
widgetResultJust x = do
    toWidget [hamlet|
    
         $forall receita <- x
            #{receita}<br>
        
    |]