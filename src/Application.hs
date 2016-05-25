{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import System.FilePath ( (</>) ) 
import Control.Lens (makeLenses) 
import Snap.Snaplet
import Snap.Snaplet.Heist
-- import Snap.Snaplet.Auth
import Snap.Snaplet.Session
-- import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)

import qualified Cda.Types as CdaT

------------------------------------------------------------------------------

data App = App  
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _viewer :: Snaplet CdaT.CdaViewer
  --, _ajax :: Snaplet Ajax
  }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------

type AppHandler = Handler App App

------------------------------------------------------------------------------
viewerInit:: SnapletInit b CdaT.CdaViewer
viewerInit = makeSnaplet "viewer" "Cda viewer snaplet" Nothing $ do
    cwd <- liftIO $ getCurrentDirectory
    let tmpdir = cwd </> "tmp"
    liftIO $ createDirectoryIfMissing False tmpdir
    return (CdaT.CdaViewer tmpdir)

{--ajaxInit :: SnapletLens b Home -> SnapletInit b Ajax
--ajaxInit _h = makeSnaplet "ajax" "Ajax snaplet" Nothing $ do
ajaxInit :: SnapletInit b Ajax
ajaxInit = makeSnaplet "ajax" "Ajax snaplet" Nothing $ do
     return Ajax
-}

