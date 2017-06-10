{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Utils.SettingsForm where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text
import Yesod.Form

-- campo 1: Busca por ingredientes excluidos da receita
-- campo 2: Busca por sabor
-- campo 3: Busca por nome da receita, ingredientes ou categorias
data Busca = Busca
    { buscaCampo1   :: Maybe Text
    , buscaCampo2   :: Sabor
    , buscaCampo3   :: Maybe Text
    }
    deriving Show

data Sabor = Todos | Amargo | Agridoce | Azedo | Doce | Salgado | Umami
    deriving (Show, Eq, Enum, Bounded)

   
withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("placeholder", placeholder) : fsAttrs fs

bfs :: RenderMessage site msg => msg -> Text -> FieldSettings site
bfs msg s = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", s)]

-- areq: required | aopt: optional
form :: Form Busca
form = renderDivs $ Busca
    <$> aopt textField (campoSettings "exemplo: leite, soja " "\nExcluir ingrediente(s): " "select") Nothing
    <*> areq (selectFieldList sabores) (campoSelect "\nSabor: " "select qb-linha") Nothing
    <*> aopt textField (campoSettings "Receita ou ingrediente" "\n" "form-busca input qb-linha") Nothing
  where
    campoSettings x y z = withPlaceholder (pack x) $ 
                   (bfs (y :: Text) z)
    campoSelect x y = bfs (pack x) (pack y)
    sabores :: [(Text, Sabor)]
    sabores = [("Todos", Todos), ("Amargo", Amargo), ("Agridoce", Agridoce), ("Azedo", Azedo), ("Doce", Doce), ("Salgado", Salgado), ("Umami", Umami)]
    
    