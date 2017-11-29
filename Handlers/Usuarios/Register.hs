{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handlers.Usuarios.Register where

import Foundation
import Yesod
import Widgets.SettingsForm
import Widgets.PageGenericContent

getRegisterR :: Handler Html
getRegisterR = do 
    (widget, enctype) <- generateFormPost form
    (register, enctype) <- generateFormPost formRegister
    let title = "Cadastro"
    newLayout title
        [whamlet|
            ^{menu BuscaR enctype widget}    
            <div  id="container">
                <h1> #{title} </h1>
                <p> Já se cadastrou? <a href="@{LoginR}" title="cadastro"> Clique aqui</a> para ser redirecionado para a página de login.</p>
                <p> Ainda não tem cadastro? Então preencha o formulário abaixo para se cadastrar no sistema. </p>
                <div>
                    <form method=post action=@{RegisterR} enctype=#{enctype}>
                        ^{register}
                        <button type="submit" class="form-busca button">Cadastrar</button> 
            ^{footer}
        |]

postRegisterR :: Handler Html
postRegisterR = do
        (widget, enctype) <- generateFormPost form
        ((res', register), enctype) <- runFormPost formRegister
        let sucesso = "Cadastrado com sucesso!"
        let erro = "Erro! "
        case res' of 
            FormSuccess ((Just nome), email, senha, repitaSenha) -> do
                case senha == repitaSenha of
                    True -> do
                        userid <- runDB $ insert (Login email senha)
                        usid <- runDB $ insert (Usuario userid (Just nome) senha email)
                        newLayout sucesso
                            [whamlet|
                                ^{menu BuscaR enctype widget}    
                                <div id="container">
                                    <h1> Cadastro </h1>
                                    <p> #{sucesso} </p>
                                    <p> Seja bem vindo!! <a href=@{LoginR} title="login"> Clique aqui </a> para fazer login.</p>
                                ^{footer}
                            |]
                    False -> do
                        newLayout erro
                            [whamlet|
                                ^{menu BuscaR enctype widget}    
                                <div id="container">
                                    <h1> Cadastro </h1>
                                    <p> #{erro} As senhas devem ser semelhantes!!
                                    <form method=post action=@{RegisterR} enctype=#{enctype}>
                                        ^{register}
                                        <button type="submit" class="form-busca button">Cadastrar</button> 
                                ^{footer}
                            |]
            _ -> do
                newLayout erro
                    [whamlet|
                        ^{menu BuscaR enctype widget}    
                        <div id="container">
                            <h1> Cadastro </h1>
                            <p> #{erro} Preencha os campos corretamente!!
                            <form method=post action=@{RegisterR} enctype=#{enctype}>
                                ^{register}
                                <button type="submit" class="form-busca button">Cadastrar</button> 
                        ^{footer}
                    |]