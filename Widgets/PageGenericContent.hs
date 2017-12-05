{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Widgets.PageGenericContent where

import Foundation
import Yesod
import Yesod.Core
import Yesod.Static
import Data.Text
import Yesod.Auth
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2



menu :: Route App -> Enctype -> Widget -> Widget
menu rota enctype widget = $(whamletFile "templates/menu.hamlet")


footer :: Widget
footer = $(whamletFile "templates/footer.hamlet")