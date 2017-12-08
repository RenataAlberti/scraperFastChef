{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Static()
import Yesod.Form
import Widgets.SettingsForm
import Yesod.Auth
import Widgets.PageGenericContent

getHomeR :: Handler Html
getHomeR = do 
    ((a, widget), enctype) <- generateFormGet form
    maid <- maybeAuthId
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
    newLayout ("FastChef")
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
                <h1> Início </h1>
                <div class="row recipe">
                    <a href="" title="pizza-link">
                        <h2> Macarrão sabor pizza </h2>
                        <img src=@{StaticR img_pizza_jpg} alt="pizza-imagem-principal" class="img-thumb">
                        <dl>
                            <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                            <dd> 6 porções </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                            <dd>25 min </dd><br>
                            <dt><span class="margin-right"><i class="fa fa-copyright" aria-hidden="true"></i></span>  Fonte: </dt>
                            <dd> TudoGostoso </dd>
            ^{footer}
        |]