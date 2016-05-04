{-# LANGUAGE OverloadedStrings #-}

module Cda.Parser where

import qualified Data.Text as T
import Text.XML.Expat.Tree
-- import Text.XML.Expat.Format
import Text.XML.Expat.Proc
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Maybe ( fromJust, maybe )
import Control.Monad 
import Data.Monoid

strM::T.Text -> Maybe T.Text
strM s =  if (T.length s) > 0 then Just s else Nothing
---------------------------

tagTextM:: UNode T.Text -> T.Text -> Maybe T.Text
tagTextM node stag = findChild stag node >>= (strM . textContent) 
----------------------------

attrTextM:: UNode T.Text -> T.Text -> T.Text ->  Maybe T.Text
attrTextM node stag sattr = flip getAttribute sattr =<< findChild stag node
----------------------------

viewTitle:: UNode T.Text -> T.Text
viewTitle node = do
  let 
    defTitle = Just ("Clinical document"::T.Text)
    tagTitle = tagTextM node "title"
    codeName = attrTextM node "code" "displayName"
  fromJust $ tagTitle `mplus` codeName `mplus` defTitle 
----------------------------

getCsection:: UNode T.Text -> [UNode T.Text]
getCsection = findElements "component" <=< findChild "structuredBody" <=< findChild "component"

map set 


renderCda:: UNode T.Text -> T.Text
renderCda = 
