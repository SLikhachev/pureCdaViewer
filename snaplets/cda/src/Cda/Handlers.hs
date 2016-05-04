{-# LANGUAGE OverloadedStrings #-}

module Cda.Handlers where

import  System.IO
-- import System.Directory
-- import System.Environment
import  Data.Maybe ( fromJust, maybe )
-- import Control.Monad 
-- import Data.Monoid
-- import  Control.Applicative
import  Control.Monad.IO.Class as LIO
import  qualified Data.ByteString as BS
import  qualified Data.ByteString.Lazy as BL
import  qualified Data.Text as T

import  Snap.Core
import  Snap
import  Snap.Util.FileUploads
import  Snap.Snaplet
import  Snap.Snaplet.Heist
import  Snap.Iteratee (consume)
import  Snap.Snaplet.Session

-- import qualified Data.ByteString.Char8 as S
-- import  qualified Data.String.UTF8 as T8
-- import  Data.String.Conversions 
-- import  qualified  Text.XmlHtml as X
-- import  qualified Data.Aeson as A

import  Text.XML.Expat.Tree
-- import Text.XML.Expat.Format
import  Text.XML.Expat.Proc
-- import qualified Text.XML.Expat.Lens.Unqualified as XL
-- import Control.Lens 
-- import Data.Default (def)

import  Heist
import  qualified Heist.Interpreted as I
-- import  qualified Heist.Compiled as C
-- import  qualified Heist.Compiled.LowLevel as LL
import  Application

------------------------------------------------------------------------

mB = 2^(20::Int)
maxMb = 2 * mB

cdaUpl:: UploadPolicy -> UploadPolicy
-- cdaDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy
cdaUpl = (setProcessFormInputs True) . (setMaximumFormInputSize maxMb) . 
  (setMinimumUploadRate 1024) . (setMinimumUploadSeconds 3 ) . (setUploadTimeout 20)
thisUpl :: UploadPolicy
thisUpl = cdaUpl defaultUploadPolicy

cdaPerPartPolicy :: PartInfo -> PartUploadPolicy
cdaPerPartPolicy _ = allowWithMaximumSize maxMb
{-
data PartInfo =
    PartInfo { partFieldName   :: !ByteString
             , partFileName    :: !(Maybe ByteString)
             , partContentType :: !ByteString
             }
-}
tempDir::String
tempDir = "c:/yesod/cda/tmp"

uploadFiles :: AppHandler ()
uploadFiles = do
  files <- handleMultipart thisUpl $ \part -> do
    content <-  liftM BS.concat consume
    return (part, content)
  let fs = null files
  -- liftIO $ putStrLn $ show fs
  let r = if fs then "/empty" else parseCda (head files)
 
  redirect r

  -- render "parse_error"

parseCda:: (PartInfo, BS.ByteString) -> BS.ByteString
parseCda (partInfo, inputText) = 
  let 
    p = parse' defaultParseOptions inputText :: Either XMLParseError (UNode T.Text)
  in
  case p of
    Right cda -> renderCda cda
    Left err -> "/error"
{-
    renderWithSplices "parse_error" ("error" ## I.textSplice text)
                where
                  text = T.pack $ show err

viewF:: (Monad m, MonadIO m) => [(PartInfo, BS.ByteString)] -> m (Splices (SnapletISplice App))
viewF fs = do
  let
    ff = "files" ## (files fs)
    files = I.mapSplices $ I.runChildrenWith . fromEt
    fromEt (p, c) = "file" ## I.textSplice $ T.pack $ "herr"
  return ff
-}




{-    
-------------------------------------------------------------------------------
-- | Handle Ajax page
ajaxHandler :: Handler App Ajax ()
ajaxHandler = do
    page <- getParam "page" 
    case page of
        Just page -> let p = ("ajax/"::BS.ByteString) <> page in render p
        Nothing -> return ()


-------------------------------------------------------------------------------
-}

