{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cda.Types where

import Control.Lens

data CdaViewer = CdaViewer {
    _tmpDir :: FilePath
}

makeLenses  ''CdaViewer
