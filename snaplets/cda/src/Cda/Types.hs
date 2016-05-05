{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Cda.Types where

import qualified Data.Text as T
import Control.Lens
import System.Directory (getCurrentDirectory)

data CdaViewer = CdaViewer {
    _tmpDir :: FilePath
}

makeLenses  ''CdaViewer
{-
data CdaDoc = CdaDoc {
  
  _cda_Title:: T.Text,
  _cda_Patient:: [Patient],
  _cda_Section:: [Section]

} deriving Show

data Patient = Patient {
    _pt_name:: PatName,
    _pt_birthTime:: T.Text, 
    _pt_gender:: T.Text,
    _pt_race:: T.Text,
    _pt_ethnos:: T.Text,
    _pt_addr:: PatAddr,
    _pt_tel:: T.Text,
    _pt_id:: [T.Text]
} deriving Show

data PatName = PatName {
    _pn_prefix::T.Text, 
    _pn_given:: T.Text, 
    _pn_family::T.Text, 
    _pn_suffix:: T.Text 
} deriving Show

data PatAddr = PatAddr {
    _pa_addr::T.Text
} deriving Show

data Section = Section {
    _sec_title::T.Text
} deriving Show



makeLenses ''CdaDoc
makeLenses ''Patient
makeLenses ''PatName
makeLenses ''Section
-}