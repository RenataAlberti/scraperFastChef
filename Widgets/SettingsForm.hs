{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Widgets.SettingsForm where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text
import Yesod.Form


data Busca = Busca { q   :: Maybe Text } deriving Show

data Email = Email { emailcadastro :: Text } deriving Show

data RedefinirSenha = RedefinirSenha
    { novaSenha :: Text
    , confNovaSenha :: Text
    }
    deriving Show

data Prefav = Prefav
    { prefavNome        :: Text
    , prefavTempo       :: Text
    , prefavRendimento  :: Text
    , prefavUrl         :: Text
    , prefavUrlimg      :: Text
    , prefavUrlfonte    :: Text
    , prefavNomefonte   :: Text
    }deriving Show

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
    <$> aopt (searchField True) (settings "Digite a receita, categoria ou ingrediente aqui" "" "form-busca input qb-linha") Nothing
 
-- Handler Usuarios/Register    
formRegister :: Form (Maybe Text, Text, Text, Text)
formRegister = renderDivs $ (,,,)
    <$> aopt textField (withAutofocus (settings "digite seu nome ou apelido" "\n*Nome: " "form-register")) Nothing
    <*> areq emailField (settings "digite seu e-mail" "\n*E-mail:" "form-register") Nothing
    <*> areq passwordField  (settings "digite uma senha" "\n*Senha: " "form-register") Nothing
    <*> areq passwordField  (settings "repita a senha" "\n*Repita a senha: " "form-register") Nothing

-- Handler Usuarios/Login   
formLogin :: Form Login
formLogin = renderDivs $ Login
    <$> areq emailField (withAutofocus (settings "digite seu e-mail aqui" "\n*E-mail: " "form-register")) Nothing
    <*> areq passwordField (settings "digite sua senha aqui" "\n*Senha: " "form-register") Nothing
    
formEmail :: Form Email
formEmail = renderDivs $ Email
    <$> areq emailField (withAutofocus (settings "digite o e-mail cadastrado" "\nE-mail: " "form-control")) Nothing