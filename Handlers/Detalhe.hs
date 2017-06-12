{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.Detalhe where

import Foundation
import Yesod.Core
import Yesod.Static()
import Data.Text(Text, pack, unpack)
import Yesod.Form
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Utils.WidgetResultadoBusca
import Utils.SettingsForm
import Handlers.Busca
import Scraper.Ztestes.Boott
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import Control.Monad.IO.Class()
import Text.Taggy.Lens (html, attr, contents, element, allNamed, attributed, content, children)
import Text.Taggy.DOM
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack, unpack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE

-- Lentes
rendimento = allNamed (only "data") . children . traverse . content

tempopreparo = allNamed (only "time") . contents

nomereceita = allNamed (only "h1") . children . traverse . content

nomecategoria = allNamed (only "a") . attributed (ix "property" . only "v:title") . element . children . traverse . content

caminhofotos = allNamed(only "img") . attributed (ix "class" . only "pic") . attr "src" . _Just

linkoriginal = allNamed (only "meta") . attributed (ix "name" . only "twitter:url") . attr "content" .  _Just

nomeautoria = allNamed(only "meta") . attributed (ix "name" . only "twitter:app:name:ipad" ) . attr "content" . _Just

ingrspan = allNamed(only "div") . attributed (ix "id" . only "info-user") . to universe . traverse . contents

mdpreparo = allNamed(only "h3") . attributed (ix "class" . only "directions-title box-title") . children . traverse . content

infadd = allNamed(only "div") . attributed (ix "class" . only "instructions e-instructions") . to universe . traverse . contents

-- funcoes aleatorias
funfunfun = fmap (unpack . Prelude.head)

funfun = fmap (unpack . Prelude.last)

--lista x = fmap (intercalate (pack x))

getDetalheR :: Handler Html
getDetalheR = do
    (widget, enctype) <- generateFormPost form
    defaultLayout $ do
        page <-  return $ header' 
        rend        <- liftIO $ funfunfun   $ page rendimento     -- rendimento
        tempo       <- liftIO $ funfunfun   $ page tempopreparo   -- tempo de preparo
        autoria     <- liftIO $ funfunfun   $ page nomeautoria    -- copyright
        linko       <- liftIO $ funfunfun   $ page linkoriginal   -- link da receita original
        nmreceita   <- liftIO $ funfunfun   $ page nomereceita    -- nome da receita
        nmcat       <- liftIO $ funfun      $ page nomecategoria  -- nome da categoria da receita
        fotos       <- liftIO $ funfunfun   $ page caminhofotos   -- primeira foto da receita
        fotos'      <- liftIO $ funfun      $ page caminhofotos   -- segunda foto da receita
        ingsp       <- liftIO $ fmap (Prelude.takeWhile (\x -> x /= (pack "\nEnviada por\n"))) $ page ingrspan                 -- lista dos ingredientes
        mdp         <- liftIO $ funfun   $ page mdpreparo      -- modo de preparo
        infor       <- liftIO $ page infadd
        setTitle "FastChef"
       
        toWidgetHead[hamlet|
            <meta http-equiv="X-UA-Compatible" content="IE=edge">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <meta charset="UTF-8">
        |]
    
        -- Adicionando o FontAwesome
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    
        -- Adicionando a folha de estilos
        addStylesheet $ StaticR css_estilo_css
    
        -- Adicionando o jquery via CDN
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"
    
        [whamlet|
                <header> 
                    <nav id="menu">
                        <ul>
                            <li><img src=@{StaticR img_logovertical_png} id="logo" alt="logo-fastchef">
                            <li>
                                <form method=post action=@{BuscaR} enctype=#{enctype}>
                                  ^{widget}
                                 <button type="submit" class="form-busca button"><i class="fa fa-search" aria-hidden="true"></i></button> 
                <div id="containerview">
                <h1>#{Prelude.tail nmreceita}</h1>
                <aside>
                    <img src=#{Prelude.reverse (Prelude.drop 30 (Prelude.reverse fotos))} alt=#{Prelude.tail nmreceita} class="img-receita">
                <div>
                    <dl>
                        <dt><span class="margin-right"><i class="fa fa-cutlery" aria-hidden="true"></i></span>  Rendimento: </dt>
                            <dd> #{rend} <dd><br>
                        <dt><span class="margin-right"><i class="fa fa-clock-o" aria-hidden="true"></i></span>  Tempo de preparo: </dt>
                            <dd> #{tempo} </dd><br>
                        <dt><span class="margin-right"><i class="fa fa-copyright" aria-hidden="true"></i></span>  Fonte: </dt>
                            <dd> #{autoria} </dd>
                        <dt> <span class="margin-right"> <i class="fa fa-external-link" aria-hidden="true"></i> </span> Link da receita original: </dt>
                            <dd> <a href=#{linko} title="-link-receita-original"> #{linko} </a></dd>
                        <h3> #{Prelude.head ingsp} </h3>
                        <p>
                            $forall ings <- (Prelude.tail ingsp)
                                #{ings} <br>
                        
                        <h3> #{Prelude.head infor}</h3>
                        <p>
                            $forall infs <- (Prelude.tail infor)
                                #{infs}<br>
        |]