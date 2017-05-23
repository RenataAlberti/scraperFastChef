{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod.Core
import Yesod.Static
import Control.Applicative()
import Data.Text()
import Yesod.Form

-- static
staticFiles "static"

-- importanto o getStatic
data App = App {getStatic :: Static}

-- sei la pra que eh
mkYesodData "App" $(parseRoutesFile "routes")

-- sei la pra que eh
instance Yesod App

-- Formulario
type Form a = Html -> MForm Handler (FormResult a, Widget)

-- Formulario
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
