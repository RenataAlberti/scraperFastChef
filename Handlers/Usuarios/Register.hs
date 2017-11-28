{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Handlers.Usuarios.Register where

import Foundation
import Yesod.Core
import Data.Text
import Yesod.Form
import Widgets.SettingsForm
import Widgets.WidgetResultadoBusca
import Widgets.PageGenericContent

getRegisterR :: Handler Html
getRegisterR = do 
    (widget, enctype) <- generateFormPost form
    (register, enctype) <- generateFormPost formRegister
    let title = "Cadastro"
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
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
                <p> Já se cadastrou? <a href="@{LoginR}" title="cadastro"> Clique aqui</a> para ser redirecionado para a página de login.</p>
                <p> Ainda não tem cadastro? Então preencha o formulário abaixo para se cadastrar no sistema. </p>
                <div>
                    <form method=post action=@{RegisterR} enctype=#{enctype}>
                        ^{register}
                        <button type="submit" class="form-busca button">Cadastrar</button> 

            ^{footer}
        |]

postRegisterR :: Handler Html
postRegisterR = undefined