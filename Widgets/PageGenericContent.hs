{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Widgets.PageGenericContent where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text

footer :: Widget
footer = $(whamletFile "templates/footer.hamlet")