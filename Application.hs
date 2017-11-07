{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Home
import Handlers.Busca
import Handlers.Detalhe
import Handlers.Sobre
import Utils.WidgetResultadoBusca()
import Utils.SettingsForm()
import Scraper.Ztestes.Viii
import Scraper.Ztestes.Boott
import Scraper.Ztestes.Scraper()
import Scraper.Ztestes.Teste
import Scraper.Ztestes.Secondtest
import Handlers.Jsonnaveia
import Handlers.ViewDetails

mkYesodDispatch "App" resourcesApp

