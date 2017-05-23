{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Scraper.Ztestes.Viii where

import Foundation
import Yesod.Core
import Yesod.Static()
import Control.Lens hiding (children, element)
import Data.Aeson()
import Data.Aeson.Lens()
import Network.Wreq
import Control.Monad.IO.Class()
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, allNamed, attributed, content, children)
import Text.Taggy.DOM
import qualified Network.Wreq.Session as S
import Data.String.UTF8
import qualified Data.Text.Encoding as DTE
import qualified Data.List as DL
import Data.Text as T
import Data.Aeson.Lens (_String, key)

header' func = do
            let opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36"]
                           & header "Accept" .~ ["text/html, */*"]
                           & header "X-Requested-With" .~ ["XMLHttpRequest"]
                           & header "Accept-Language" .~ ["pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4"]
                           & header "Accept-Encoding" .~ ["gzip, deflate"]
                           & header "Referer" .~ ["https://www.google.com.br/search?q=tudo+gostoso&rlz=1C1AVFB_enBR731BR732&oq=tudo+gostoso+&aqs=chrome..69i57j69i60l3j0l2.11480j0j8&sourceid=chrome&ie=UTF-8"]
                           & header "Origin" .~ ["http://www.google.com.br"]
                           & header "Connection" .~ ["keep-alive"]
                           & param "q" .~ ["creme"]
            r <- S.withSession $ \sess -> do
                S.getWith opts sess "http://www.tudogostoso.com.br/busca.php"
            return $ r ^. responseHeader "Content-Type"

--temporeceitas = allNamed(only "span") . attributed (ix "class" . only "time recipe-info") . children 

--rendreceitas = allNamed(only "span") . attributed (ix "class" . only "portion recipe-info") . children 

--imagemreceitas = allNamed(only "span") . attributed (ix "class" . only "photo") . children

--funfunfun = fmap (unpack . Prelude.head )
--funfun = fmap (unpack . Prelude.last)
--listaVirgula = fmap (intercalate (pack ","))


getViiiR :: Handler Html
getViiiR = defaultLayout $ do
    -- setTitle "FastChef"
    -- rend        <- liftIO $ funfunfun $ header' rendimento 
    -- tempo       <- liftIO $ header' tempopreparo
    -- autoria     <- liftIO $ header' nomeautoria
    -- linko       <- liftIO $ header' linkoriginal
    -- nmreceita   <- liftIO $ header' nomereceita
    -- nmcat       <- liftIO $ header' nomecategoria
    -- fotos       <- liftIO $ header' caminhofotos
    -- ing         <- liftIO $ listaVirgula $ header' ingrh3
    -- ingsp       <- liftIO $ header' ingrspan

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
        <div  id="container">
            <h1> Início </h1>
            <section>
                <a href=@{BoottR} title="testetdh">
    |]
