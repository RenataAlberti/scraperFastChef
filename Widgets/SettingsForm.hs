{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Widgets.SettingsForm where

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

data Sabor = Todos | Agridoce | Doce | Salgado
    deriving (Show, Eq, Enum, Bounded)

data Email = Email { emailcadastro :: Text } deriving Show

data RedefinirSenha = RedefinirSenha
    { novaSenha :: Text
    , confNovaSenha :: Text
    }
    deriving Show

{- Configurações de placeholder -}   
withHidden :: Text -> FieldSettings site -> FieldSettings site
withHidden hidden fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("hidden", hidden) : fsAttrs fs

withAutofocus :: FieldSettings site -> FieldSettings site
withAutofocus fs = fs {fsName = (Just "autofocus")}

withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
    where newAttrs = ("placeholder", placeholder) : fsAttrs fs

bfs :: RenderMessage site msg => msg -> Text -> FieldSettings site
bfs msg s = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", s)]

settings :: String -> Text -> Text -> FieldSettings site
settings x y z = withPlaceholder (pack x) $ (bfs (y :: Text) z)

settingsHidden :: String -> Text -> Text -> FieldSettings site
settingsHidden x y z = withHidden (pack x) $ (bfs (y :: Text) z)

{- Formulario de busca, cadastro e login -}
-- areq: required | aopt: optional
-- Handler (todas, formulario de busca)
form :: Form Busca
form = renderDivs $ Busca
    <$> aopt textField (settings "exemplo: leite, soja " "\nExcluir ingrediente(s): " "select") Nothing
    <*> areq (selectFieldList sabores) (campoSelect "\nSabor: " "select qb-linha") Nothing
    <*> aopt textField (withAutofocus (settings "Receita ou ingrediente" "" "form-busca input qb-linha")) Nothing
  where
    campoSelect x y = bfs (pack x) (pack y)
    sabores :: [(Text, Sabor)]
    sabores = [("Todos", Todos), ("Agridoce", Agridoce), ("Doce", Doce), ("Salgado", Salgado)]

-- Handler Usuarios/Register    
formRegister :: Form (Maybe Text, Text, Text, Text)
formRegister = renderDivs $ (,,,)
    <$> aopt textField (withAutofocus (settings "digite seu nome ou apelido" "\nNome: " "form-busca input qb-linha")) Nothing
    <*> areq emailField (settings "digite seu e-mail" "\nE-mail:" "input qb-linha") Nothing
    <*> areq passwordField  (settings "digite uma senha" "\nSenha: " "") Nothing
    <*> areq passwordField  (settings "repita a senha que você criou" "\nRepita a senha: " "") Nothing

-- Handler Usuarios/Login   
formLogin :: Form Login
formLogin = renderDivs $ Login
    <$> areq emailField (withAutofocus (settings "digite seu e-mail aqui" "\nE-mail: " "form-control")) Nothing
    <*> areq passwordField (settings "digite sua senha aqui" "\nSenha: " "form-control") Nothing
    
formEmail :: Form Email
formEmail = renderDivs $ Email
    <$> areq emailField (withAutofocus (settings "digite o e-mail cadastrado" "\nE-mail: " "form-control")) Nothing

-- Handler Usuarios/Favoritos
formEdit :: Form (Text, Text, Text, Text)
formEdit = renderDivsNoLabels $ (,,,)
    <$> areq textField (settingsHidden "true" "" "form-control") Nothing
    <*> areq textField (settingsHidden "true" "" "form-control") Nothing
    <*> areq textField (settingsHidden "true" "" "form-control") Nothing
    <*> areq textField (settingsHidden "true" "" "form-control") Nothing