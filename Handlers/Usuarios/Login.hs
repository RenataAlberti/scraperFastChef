{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module Handlers.Usuarios.Login where

import Foundation
import Yesod
import Data.Text
import Widgets.SettingsForm
import Widgets.PageGenericContent
import Yesod.Auth
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2

getLooginR :: Handler Html
getLooginR = do 
    ((a, widget), enctype) <- generateFormGet form
    (login, enctype) <- generateFormPost formLogin
    maid <- maybeAuthId
    msg <- getMessage
    let title = "Login"
    newLayout title
        [whamlet|
            <header> 
                <nav id="menu">
                    <div id="naveg">
                        <ul>
                            $maybe _ <- maid
                                <li> 
                                    <a href=@{ListarFavR} title="favoritos"> Favoritos
                                <li> 
                                    <a href=@{LoogoutR} title="logout"> Logout
                            $nothing
                                <li> 
                                    <a href=@{RegisterR} title="cadastro"> Cadastro
                                <li> 
                                    <a href=@{LooginR} title="login"> Login
                    ^{menu BuscaR enctype widget}
            <div  id="container">
                <h1> #{title} </h1>
                $maybe _ <- maid
                    <p class="alert">Hey! Você já fez login.
                    <p> <a href=@{LoogoutR} title="logout" class="link"> Clique aqui </a> para sair da sua conta.
                $nothing
                    $maybe mensagem <- msg
                        #{mensagem}
                    $nothing
                        <p> Ainda não tem cadastro? <a href="@{RegisterR}" title="cadastro" class="link"> Clique aqui</a> e faça seu cadastro.</p>
                        <p> Já se cadastrou? Então preencha o formulário abaixo para entrar no sistema. </p>
                    <div>
                        <form method=post action=@{LooginR} enctype=#{enctype}>
                            ^{login}
                            <button type="submit" class="linkbtn">Entrar</button> 
            ^{footer}
        |]

postLooginR :: Handler Html
postLooginR = do
    ((a, widget), enctype) <- generateFormGet form
    ((res', login), enctype) <- runFormPost formLogin
    maid <- maybeAuthId
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
                                    <div id="naveg">
                                        <ul>
                                            $maybe _ <- maid
                                                <li> 
                                                    <a href=@{ListarFavR} title="favoritos"> Favoritos
                                                <li> 
                                                    <a href=@{LoogoutR} title="logout"> Logout
                                            $nothing
                                                <li> 
                                                    <a href=@{RegisterR} title="cadastro"> Cadastro
                                                <li> 
                                                    <a href=@{LooginR} title="login"> Login
                                    ^{menu BuscaR enctype widget}
                            <div  id="container">
                                <h1> Login </h1>
                                <p class="alert">#{erro} Usuário ou senha incorreta.
                                <p> Ainda não tem cadastro? <a href=@{RegisterR} title="cadastro" class="link"> Clique aqui</a> e faça seu cadastro.</p>
                                <form method=post action=@{LooginR} enctype=#{enctype}>
                                    ^{login}
                                    <button type="submit" class="linkbtn">Entrar</button> 
                        |]
                Just (Entity uid uEmail) -> do
                    setSession "_USER" (pack $ show uid)
                    redirect ListarFavR
        _ -> do
            newLayout erro
                [whamlet|
                    <header> 
                        <nav id="menu">
                            <div id="naveg">
                                <ul>
                                    $maybe _ <- maid
                                        <li> 
                                            <a href=@{ListarFavR} title="favoritos"> Favoritos
                                        <li> 
                                            <a href=@{LoogoutR} title="logout"> Logout
                                    $nothing
                                        <li> 
                                            <a href=@{RegisterR} title="cadastro"> Cadastro
                                        <li> 
                                            <a href=@{LooginR} title="login"> Login
                            ^{menu BuscaR enctype widget}
                    <div  id="container">
                        <h1> Login </h1>
                        <p class="alert">#{erro} Preencha os campos corretamente.
                        <p> Ainda não tem cadastro? <a href=@{RegisterR} title="cadastro" class="link"> Clique aqui</a> e faça seu cadastro.</p>
                        <form method=post action=@{LooginR} enctype=#{enctype}>
                            ^{login}
                            <button type="submit" class="linkbtn">Entrar</button>
                |]

getLoogoutR :: Handler Html
getLoogoutR = do
    ((a, widget), enctype) <- generateFormGet form
    deleteSession "_USER"
    redirect LooginR

getRecuperaSenhaR :: Handler Html
getRecuperaSenhaR = do 
    ((a, widget), enctype) <- generateFormGet form
    (email, enctype) <- generateFormPost formEmail
    maid <- maybeAuthId
    let title = "Recuperação de senha"
    newLayout title
        [whamlet|
            <header> 
                <nav id="menu">
                    <div id="naveg">
                        <ul>
                            $maybe _ <- maid
                                <li> 
                                    <a href=@{ListarFavR} title="favoritos"> Favoritos
                                <li> 
                                    <a href=@{LoogoutR} title="logout"> Logout
                            $nothing
                                <li> 
                                    <a href=@{RegisterR} title="cadastro"> Cadastro
                                <li> 
                                    <a href=@{LooginR} title="login"> Login
                    ^{menu BuscaR enctype widget}
            <div  id="container">
                <h1> #{title} </h1>
                <p> Lembrou sua senha? <a href=@{LooginR} title="login" class="link"> Clique aqui </a> para fazer login. </p>
                <div>
                    <form method=post action=@{RecuperaSenhaR} enctype=#{enctype}>
                        ^{email}
                        <button type="submit" class="linkbtn">Entrar</button> 
            ^{footer}
        |]

postRecuperaSenhaR :: Handler Html
postRecuperaSenhaR = undefined