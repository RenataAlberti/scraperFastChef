{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.Detalhe where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text
import Text.Taggy 
import Text.Taggy.Lens 
import Scraper.Busca.Receita


-- "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"

getDetalheR :: Handler Html
getDetalheR = defaultLayout $ do 
        detalhe <- liftIO $ comLet
        setTitle "FastChef"
        
        [whamlet|
           #{Prelude.map (toMarkup False) detalhe}
        |]
    