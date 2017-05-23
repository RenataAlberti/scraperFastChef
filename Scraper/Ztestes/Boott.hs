{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Ztestes.Boott where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import Control.Monad.IO.Class()
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, allNamed, attributed, content, children)
import Text.Taggy.DOM
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import qualified Data.List as DL
import Data.Text as T
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack, unpack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding as TE


urlDireto = unpack "http://www.tudogostoso.com.br/receita/62547-a-melhor-receita-de-bolo-de-chocolate.html"
  
header' func = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess urlDireto
            return $ r ^.. responseBody . to LE.decodeUtf8 . html . func
            
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

comLet = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess urlDireto
            let fullBody = (r ^. responseBody . to LE.decodeUtf8)
            let rend = (fullBody ^.. html . allNamed (only "data") . children . traverse . content)
            let temp = (fullBody ^.. html . allNamed (only "time") . contents)
            let nmre = (fullBody ^.. html . allNamed (only "h1") . children . traverse . content)
            let nmca = (fullBody ^.. html . allNamed (only "a") . attributed (ix "property" . only "v:title") . element . children . traverse . content)
            let cafo = (fullBody ^.. html . allNamed(only "img") . attributed (ix "class" . only "pic") . attr "src" . _Just)
            let lior = (fullBody ^.. html . allNamed (only "meta") . attributed (ix "name" . only "twitter:url") . attr "content" .  _Just)
            let nmau = (fullBody ^.. html . allNamed(only "meta") . attributed (ix "name" . only "twitter:app:name:ipad" ) . attr "content" . _Just)
            let insp = (fullBody ^.. html . allNamed(only "div") . attributed (ix "id" . only "info-user") . to universe . traverse . contents)
            let mdpr = (fullBody ^.. html . allNamed(only "h3") . attributed (ix "class" . only "directions-title box-title") . children . traverse . content)
            let inad = (fullBody ^.. html . allNamed(only "div") . attributed (ix "class" . only "instructions e-instructions") . to universe . traverse . contents)
            return $  DL.concat [rend, temp, nmre, nmca, cafo, lior, nmau, insp, mdpr, inad]

-- Text -> String e exibe somente o primeiro elemento da lista
funfunfun = fmap (unpack . Prelude.head)

-- Text -> String e exibe somente o ultimo elemento da lista
funfun = fmap (unpack . Prelude.last)

-- [Text] -> Text e inclue o elemento passado por parametro intercalando entre os elementos da lista
lista x = fmap (intercalate (pack x))

getBoottR :: Handler Html
getBoottR = defaultLayout $ do
    page <-  return $ header' 
    rend        <- liftIO $ funfunfun   $ page rendimento     -- rendimento
    tempo       <- liftIO $ funfunfun   $ page tempopreparo   -- tempo de preparo
    autoria     <- liftIO $ funfunfun   $ page nomeautoria    -- copyright
    linko       <- liftIO $ funfunfun   $ page linkoriginal   -- link da receita original
    nmreceita   <- liftIO $ funfunfun   $ page nomereceita    -- nome da receita
    nmcat       <- liftIO $ funfun      $ page nomecategoria  -- nome da categoria da receita
    fotos       <- liftIO $ funfunfun   $ page caminhofotos   -- primeira foto da receita
    fotos'      <- liftIO $ funfun      $ page caminhofotos   -- segunda foto da receita
    ingsp       <- liftIO $ page ingrspan                     -- lista dos ingredientes
    mdp         <- liftIO $ lista ":"   $ page mdpreparo      -- modo de preparo
    infor       <- liftIO $ page infadd
    ct          <- liftIO $ lista ":" $ comLet
    
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
                    <li> ----------------------------------------------
        <div id="containerview">
            <h1>#{Prelude.tail nmreceita}</h1>
            <aside>
                <img src=#{Prelude.reverse (Prelude.drop 30 (Prelude.reverse fotos))} alt="pizza-imagem-principal" class="img-receita">
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
                    
                    <p> --------------------------------------------------------------------------------------------------- 
                    <p>RESULTADO COM LET
                    <p> #{ct}
    |]


curlIt = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["http://www.tudogostoso.com.br/busca.php?q=bolo+cenoura"]
                           & header "Origin" .~ ["http://www.tudogostoso.com.br"]
                           & header "Connection" .~ ["keep-alive"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.tudogostoso.com.br/receita/23-bolo-de-cenoura.html"
            return $ r ^.. responseBody . to LE.decodeUtf8 . html 