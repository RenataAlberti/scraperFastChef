{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Usuarios.Login where

import Foundation
import Yesod.Core
import Data.Text
import Yesod.Form
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
postLoginR = undefined

getRecuperaSenhaR :: Handler Html
getRecuperaSenhaR = do 
    (widget, enctype) <- generateFormPost form
    (login, enctype) <- generateFormPost formLogin
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
                <p> </p>
                <p> Já se cadastrou? Então preencha o formulário abaixo para entrar no sistema. </p>
                <div>
                    <form method=post action=@{LoginR} enctype=#{enctype}>
                        ^{login}
                        <button type="submit" class="form-busca button">Entrar</button> 

            ^{footer}
        |]

postRecuperaSenhaR :: Handler Html
postRecuperaSenhaR = undefined