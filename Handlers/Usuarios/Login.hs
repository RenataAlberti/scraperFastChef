{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Login where

import Foundation
import Yesod
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent

getLoginR :: Handler Html
getLoginR = do 
    (widget, enctype) <- generateFormPost form
    (login, enctype) <- generateFormPost formLogin
    let title = "Login"
    newLayout title
        [whamlet|
            <header> 
                <nav id="menu">
                    <ul>
                        <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                        <li>
                            <form method=post action=@{BuscaR} enctype=#{enctype}>
                                ^{widget}
                                <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
            <div  id="container">
                <h1> #{title} </h1>
                <p> Ainda não tem cadastro? <a href="@{RegisterR}" title="cadastro"> Clique aqui</a> e faça seu cadastro.</p>
                <p> Já se cadastrou? Então preencha o formulário abaixo para entrar no sistema. </p>
                <div>
                    <form method=post action=@{LoginR} enctype=#{enctype}>
                        ^{login}
                        <button type="submit" class="form-busca button">Entrar</button> 
            ^{footer}
        |]

postLoginR :: Handler Html
postLoginR = do
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
                            <header> 
                                <nav id="menu">
                                    <ul>
                                        <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                                        <li>
                                            <form method=post action=@{BuscaR} enctype=#{enctype}>
                                                ^{widget}
                                                <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                            <div  id="container">
                                <h1> Login </h1>
                                <p>#{erro} Usuário ou senha incorreta.
                                <form method=post action=@{LoginR} enctype=#{enctype}>
                                    ^{login}
                                    <button type="submit" class="form-busca button">Entrar</button> 
                        |]
                Just (Entity uid jEmail) -> do
                    setSession "_ID" (pack $ show uid)
                    newLayout "Sessão iniciada"
                        [whamlet|
                            <a href=@{LogoutR} title="encerrar sessao"> Sair </a>
                        |]
        _ -> do
            newLayout erro
                [whamlet|
                    <header> 
                        <nav id="menu">
                            <ul>
                                <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                                <li>
                                    <form method=post action=@{BuscaR} enctype=#{enctype}>
                                        ^{widget}
                                        <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                    <div  id="container">
                        <h1> Login </h1>
                        <p>#{erro} Preencha os campos corretamente.
                        <form method=post action=@{LoginR} enctype=#{enctype}>
                            ^{login}
                            <button type="submit" class="form-busca button">Entrar</button>
                |]

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    newLayout "Sessão encerrada"
        [whamlet|
            <h1> Sessao encerrada!</h1>
        |]

getRecuperaSenhaR :: Handler Html
getRecuperaSenhaR = do 
    (widget, enctype) <- generateFormPost form
    (email, enctype) <- generateFormPost formEmail
    let title = "Recuperação de senha"
    newLayout title
        [whamlet|
            <header> 
                <nav id="menu">
                    <ul>
                        <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                        <li>
                            <form method=post action=@{BuscaR} enctype=#{enctype}>
                                ^{widget}
                                <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                
            <div  id="container">
                <h1> #{title} </h1>
                <p> Lembrou sua senha? <a href=@{LoginR} title=""> Clique aqui </a> para fazer login. </p>
                <div>
                    <form method=post action=@{RecuperaSenhaR} enctype=#{enctype}>
                        ^{email}
                        <button type="submit" class="form-busca button">Entrar</button> 
            ^{footer}
        |]

postRecuperaSenhaR :: Handler Html
postRecuperaSenhaR = undefined
                    
                
                
