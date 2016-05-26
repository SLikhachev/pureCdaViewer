{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes 
module Site
  ( app
  ) where

------------------------------------------------------------------------------
-- import  Control.Applicative
import  Data.ByteString (ByteString)
-- import  Data.Monoid
-- import  qualified Data.Text as T
import  Snap.Core
import  Snap.Snaplet
-- import  Snap.Snaplet.Auth
-- import  Snap.Snaplet.Auth.Backends.MongoDB
-- import  Control.Lens 
import  Snap.Snaplet.Heist
import  Snap.Snaplet.Session.Backends.CookieSession
import  Snap.Util.FileServe
-- import  Heist
-- import  qualified Heist.Compiled as C
-- import  qualified Heist.Interpreted as I
-- import  Data.Configurator
------------------------------------------------------------------------------
-- import  Snap.Snaplet.MongoDB

import  Application
-- import  Logins
import  Cda.Handlers
-- import  qualified Cda.Splices.Compiled as CS

------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = 
  [
    ("/", ifTop $ homePage),
    ("/empty", render "_empty"),
    ("/toolong", render "_toolong"),
    ("/error", render "_parser_error"),
    -- ("/document", docunent),
  
    ("/upload", method POST $ uploadFiles),
    ("/statics", serveDirectory "statics"),
    ("", redirect "/")
  ]

--cSplices :: (Monad m, MonadSnap m) => Splices (C.Splice m)
--cSplices = mconcat [ uplRTSplices ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "cda" "HL7 C-CDA simple viewer application" Nothing $ do
  addRoutes routes
  
  h <- nestSnaplet "" heist $ heistInit "templates"
--  addConfig h $ set scCompiledSplices cSplices mempty
  
  s <- nestSnaplet "sess" sess $
    initCookieSessionManager "site_key.txt" "sess" (Just 155952000) -- session forever
 
  cd <- nestSnaplet "" viewer $ viewerInit
    --as <- nestSnaplet "" ajax $ ajaxInit
 
    --addAuthSplices h auth
  return $ App h s cd
