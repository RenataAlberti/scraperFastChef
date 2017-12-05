{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Static()
import Yesod.Form
import Widgets.SettingsForm
import Yesod.Auth
import Yesod
import Data.Default (def)
import Network.HTTP.Client.Conduit (Manager, newManager)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2
import Widgets.PageGenericContent

getHomeR :: Handler Html
getHomeR = do 
    (widget, enctype) <- generateFormPost form
    maid <- maybeAuthId
    -- ver  <- liftIO $ scrapDirect (unpack "/receita/417-gelatina-da-barbie.html")
    newLayout ("FastChef")
        [whamlet|
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