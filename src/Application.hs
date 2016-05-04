{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory)

import qualified Cda.Types as CdaT

------------------------------------------------------------------------------

data App = App  
  {_heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _viewer :: Snaplet CdaT.CdaViewerSnaplet
  --, _ajax :: Snaplet Ajax
  }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------

type AppHandler = Handler App App

------------------------------------------------------------------------------

viewerInit = makeSnaplet "viewer" "Cda viewer snaplet" Nothing $ do
    cwd <- liftIO $ getCurrentDirectory
    let tmpdir = "tmp"
    liftIO $ createDirectoryIfMissing False 
    return (CdaT.CdaViewerSnaplet t)

{--ajaxInit :: SnapletLens b Home -> SnapletInit b Ajax
--ajaxInit _h = makeSnaplet "ajax" "Ajax snaplet" Nothing $ do
ajaxInit :: SnapletInit b Ajax
ajaxInit = makeSnaplet "ajax" "Ajax snaplet" Nothing $ do
     return Ajax
-}

