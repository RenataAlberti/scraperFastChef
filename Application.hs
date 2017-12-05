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
import Handlers.Receitas.ViewAr
import Handlers.Usuarios.Favoritos
import Handlers.Usuarios.Login
import Handlers.Usuarios.Register
import Widgets.WidgetResultadoBusca()
import Widgets.SettingsForm()
import Widgets.PageGenericContent()


mkYesodDispatch "App" resourcesApp