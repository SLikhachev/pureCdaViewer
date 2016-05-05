{-# LANGUAGE OverloadedStrings #-}

module Cda.Handlers where

import  System.IO
import System.FilePath ( (</>), (<.>) ) 
-- import System.Environment
import  Data.Maybe ( fromJust, maybe )
-- import Control.Monad 
-- import Data.Monoid
-- import  Control.Applicative
import  Control.Monad.IO.Class as LIO
import  qualified Data.ByteString as BS
import  qualified Data.ByteString.Lazy as BL
import  qualified Data.Text as T
import  Data.Int

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

import  Data.UUID.V1 (nextUUID)
import  qualified Data.UUID as UID
import  Text.XML.Expat.Tree
-- import Text.XML.Expat.Format
import  Text.XML.Expat.Proc
-- import qualified Text.XML.Expat.Lens.Unqualified as XL
import Control.Lens ( view )
-- import Data.Default (def)

import  Heist
import  qualified Heist.Interpreted as I
-- import  qualified Heist.Compiled as C
-- import  qualified Heist.Compiled.LowLevel as LL
import  Application
import  qualified Cda.Types as VT

------------------------------------------------------------------------

-- mB = 2^(20::Int)
-- maxMb::Int
maxMb = 2 * (1048576::Int64)

cdaUpl:: UploadPolicy -> UploadPolicy
-- cdaDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy
cdaUpl = (setProcessFormInputs True) . (setMaximumFormInputSize maxMb) . 
  (setMinimumUploadRate 1024) . (setMinimumUploadSeconds 3 ) . (setUploadTimeout 20)
thisUpl :: UploadPolicy
thisUpl = cdaUpl defaultUploadPolicy
{-
cdaPerPartPolicy :: PartInfo -> PartUploadPolicy
cdaPerPartPolicy _ = allowWithMaximumSize maxMb

data PartInfo =
    PartInfo { partFieldName   :: !ByteString
             , partFileName    :: !(Maybe ByteString)
             , partContentType :: !ByteString
             }
-}

uploadFiles :: AppHandler ()
uploadFiles = do
  files <- handleMultipart thisUpl $ \part -> do
    content <-  liftM BS.concat consume
    return (part, content)
  let 
    fs = null files
    (pInf, cStr) = head files 
    r = if fs then "/empty" else parseCda cStr
  case r of
    "/document" -> do 
      vs <- with viewer $ getSnapletState
      uuid <- liftIO nextUUID
      let
        tdir = view VT.tmpDir $ view snapletValue vs
        fname = maybe "xxxxxxx" UID.toString uuid
        file = tdir </> fname <.> "xml"
      liftIO $ BS.writeFile file cStr
      with sess $ setInSession "file" file
      redirect "/document"
    otherwise -> (redirect r)
  
parseCda:: BS.ByteString -> BS.ByteString
parseCda inputText = 
  let 
    p = parse' defaultParseOptions inputText :: Either XMLParseError (UNode T.Text)
    l = fromIntegral (BS.length inputText) :: Int64
  in
  if l > maxMb then "/toolong" else 
    case p of
      Right cda -> "/document"
      Left err -> "/error"

{-  
-- liftIO $ putStrLn $ show fs
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

