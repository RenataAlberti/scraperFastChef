{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Login where

import Foundation
import Yesod
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent

getLooginR :: Handler Html
getLooginR = do 
    (widget, enctype) <- generateFormPost form
    (login, enctype) <- generateFormPost formLogin
    let title = "Login"
    newLayout title
        [whamlet|
            ^{menu BuscaR enctype widget}
            <div  id="container">
                <h1> #{title} </h1>
                <p> Ainda não tem cadastro? <a href="@{RegisterR}" title="cadastro"> Clique aqui</a> e faça seu cadastro.</p>
                <p> Já se cadastrou? Então preencha o formulário abaixo para entrar no sistema. </p>
                <div>
                    <form method=post action=@{LooginR} enctype=#{enctype}>
                        ^{login}
                        <button type="submit" class="form-busca button">Entrar</button> 
            ^{footer}
        |]

postLooginR :: Handler Html
postLooginR = do
    (widget, enctype) <- generateFormPost form
    ((res', login), enctype) <- runFormPost formLogin
    let erro = "Erro! "
    case res' of 
        FormSuccess res -> do
            usuario <- runDB $ selectFirst [LoginEmail ==. (loginEmail res), LoginSenha ==. (loginSenha res)] []
            case usuario of
                Nothing -> do
                    newLayout erro
                        [whamlet|
                            ^{menu BuscaR enctype widget}
                            <div  id="container">
                                <h1> Login </h1>
                                <p>#{erro} Usuário ou senha incorreta.
                                <p> Você ainda não se cadastrou? <a href=@{RegisterR} title="cadastro"> Clique aqui </a> e faça seu cadastro.
                                <form method=post action=@{LooginR} enctype=#{enctype}>
                                    ^{login}
                                    <button type="submit" class="form-busca button">Entrar</button> 
                        |]
                Just (Entity uid uEmail) -> do
                    setSession "_USER" (pack $ show uid)
                    newLayout "Sessão iniciada"
                        [whamlet|
                            ^{menu BuscaR enctype widget}
                            <div id="container">
                                <a href=@{LoogoutR} title="encerrar sessao"> Sair </a>
                        |]
        _ -> do
            newLayout erro
                [whamlet|
                    ^{menu BuscaR enctype widget}
                    <div  id="container">
                        <h1> Login </h1>
                        <p>#{erro} Preencha os campos corretamente.
                        <form method=post action=@{LooginR} enctype=#{enctype}>
                            ^{login}
                            <button type="submit" class="form-busca button">Entrar</button>
                |]

getLoogoutR :: Handler Html
getLoogoutR = do
    (widget, enctype) <- generateFormPost form
    deleteSession "_USER"
    newLayout "Sessão encerrada"
        [whamlet|
            ^{menu BuscaR enctype widget}
            <div  id="container">
                <h1> Sessao encerrada!</h1>
        |]

getRecuperaSenhaR :: Handler Html
getRecuperaSenhaR = do 
    (widget, enctype) <- generateFormPost form
    (email, enctype) <- generateFormPost formEmail
    let title = "Recuperação de senha"
    newLayout title
        [whamlet|
            ^{menu BuscaR enctype widget}    
            <div  id="container">
                <h1> #{title} </h1>
                <p> Lembrou sua senha? <a href=@{LooginR} title=""> Clique aqui </a> para fazer login. </p>
                <div>
                    <form method=post action=@{RecuperaSenhaR} enctype=#{enctype}>
                        ^{email}
                        <button type="submit" class="form-busca button">Entrar</button> 
            ^{footer}
        |]

postRecuperaSenhaR :: Handler Html
postRecuperaSenhaR = undefined