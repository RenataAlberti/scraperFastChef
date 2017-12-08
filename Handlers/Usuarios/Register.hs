{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handlers.Usuarios.Register where

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

getRegisterR :: Handler Html
getRegisterR = do 
    ((a, widget), enctype) <- generateFormGet form
    (register, enctype) <- generateFormPost formRegister
    maid <- maybeAuthId
    let title = "Cadastro"
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
                    <p class="alert"> Hey! Você já fez login.
                    <p> <a href=@{LoogoutR} title="logout" class="link"> Clique aqui </a> para sair da sua conta.
                $nothing
                    <p> Já se cadastrou? <a href=@{LooginR} title="cadastro" class="link"> Clique aqui</a> e faça seu login.</p>
                    <p> Ainda não tem cadastro? Então preencha o formulário abaixo para se cadastrar no sistema. </p>
                    <div>
                        <form method=post action=@{RegisterR} enctype=#{enctype}>
                            ^{register}
                            <button type="submit" class="linkbtn">Cadastrar</button> 
            ^{footer}
        |]

postRegisterR :: Handler Html
postRegisterR = do
        ((a, widget), enctype) <- generateFormGet form
        ((res', register), enctype) <- runFormPost formRegister
        maid <- maybeAuthId
        let sucesso = "Cadastrado com sucesso!"
        let erro = "Erro! "
        case res' of 
            FormSuccess ((Just nome), email, senha, repitaSenha) -> do
                uemail <- runDB $ selectFirst [LoginEmail ==. email] []
                case ((Prelude.length uemail) > 0) of
                    True -> do
                        redirect LooginR
                    False -> do
                        case senha == repitaSenha of
                            True -> do
                                userid <- runDB $ insert (Login email senha)
                                usid <- runDB $ insert (Usuario userid (Just nome) email senha)
                                setMessage[shamlet|
                                    <p class="success"> Cadastrado com sucesso!! </p>
                                    <p> Preencha o formulário abaixo para entrar no sistema.</p>
                                |]
                                redirect LooginR
                            False -> do
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
                                        <div id="container">
                                            <h1> Cadastro </h1>
                                            <p class="alert"> #{erro} As senhas devem ser semelhantes!!
                                            <p> Já se cadastrou? <a href=@{LooginR} title="cadastro" class="link"> Clique aqui</a> e faça seu login.</p>
                                            <form method=post action=@{RegisterR} enctype=#{enctype}>
                                                ^{register}
                                                <button type="submit" class="linkbtn">Cadastrar</button> 
                                        ^{footer}
                                    |]
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
                        <div id="container">
                            <h1> Cadastro </h1>
                            <p class="alert"> #{erro} Preencha os campos corretamente!!
                            <p> Já se cadastrou? <a href=@{LooginR} title="cadastro" class="link"> Clique aqui</a> e faça seu login.</p>
                            <form method=post action=@{RegisterR} enctype=#{enctype}>
                                ^{register}
                                <button type="submit" class="linkbtn">Cadastrar</button> 
                        ^{footer}
                    |]