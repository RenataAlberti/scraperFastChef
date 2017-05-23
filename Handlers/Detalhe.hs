{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.Detalhe where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text


-- "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"

getDetalheR :: Handler Html
getDetalheR = defaultLayout $ do 
        setTitle "FastChef"
        
        [whamlet|
           
        |]
    