{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Home
import Handlers.Receitas.Busca
import Handlers.Receitas.ViewDetails
import Utils.WidgetResultadoBusca()
import Utils.SettingsForm()


mkYesodDispatch "App" resourcesApp