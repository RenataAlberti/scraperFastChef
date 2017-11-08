{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Scraper.Busca.CyberCook where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Data.Text.Lazy.Encoding as LE
import Text.Taggy.Lens (html, attr, contents, element, named, name, allAttributed, allNamed, attributed, content, attrs , children, Element, Node(NodeContent))
import Control.Lens hiding (children, element)
import Scraper.Padrao
import Data.Tree
import Data.Tree.Lens
import Data.String.UTF8
import qualified Text.Taggy.Lens as TGL
import qualified Data.Text.Encoding as DTE
import qualified Text.Taggy.Renderer as Renderer
import qualified Text.XML.Cursor as XML
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Yesod.Static()
import Yesod.Core
import Foundation
import qualified Data.List as DT
import Data.Text (pack, unpack)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Aeson as Aeson




