{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cda.Handlers where

import  System.IO (putStrLn)
import  System.FilePath ( (</>), (<.>) ) 
import  System.Directory (removeFile, doesFileExist)
import  Data.Maybe ( fromJust, maybe, fromMaybe )
import  Control.Monad.Trans.Maybe
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
import  Cda.Splices
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

homePage:: AppHandler ()
homePage = withSession sess $ do
  file <-  with sess $ getFromSession "file"
  case file of 
    Nothing -> render "_empty"
    Just ft -> do
      let fs = T.unpack ft
      fe <- liftIO (doesFileExist fs)
      if not fe then render "_empty" else do
      text <- liftIO $ BS.readFile fs
      liftIO $ putStrLn fs
      let
        p = readCda text -- BS.ByteString -> Either XMLParseError (UNode T.Text)
        err _ = render "_parse_error"
        rCda cdnode = renderWithSplices "_document" $ cdaDocument cdnode
      either err rCda p

uploadFiles:: AppHandler ()
uploadFiles = withSession sess $ do
  files <- handleMultipart thisUpl $ \part -> do
    content <-  liftM BS.concat consume
    return (part, content)
  let 
    fs = null files
    (pInf, cStr) = head files 
    r = if fs then "/" else parseCda cStr
  case r of
    "/document" -> newCdaDoc cStr
    otherwise -> (redirect r)

newCdaDoc::BS.ByteString -> AppHandler ()
newCdaDoc input = do
  vs <- with viewer $ getSnapletState
  uuid <- liftIO nextUUID
  let
    tdir = view VT.tmpDir $ view snapletValue vs
    fname = maybe "xxxxxxx" UID.toString uuid
    file = tdir </> fname <.> "xml"
  liftIO $ BS.writeFile file input
  fdel <- with sess $! getFromSession "file"
  case fdel of
    Just f -> delFile f
    Nothing -> return ()
  with sess $ setInSession "file" (T.pack file) >> commitSession
  sl <- with sess $ sessionToList
  liftIO $ putStrLn $ show sl
  redirect "/"

delFile:: T.Text -> AppHandler ()
delFile f = do
  let 
    fn = T.unpack f
    rv = liftIO (removeFile fn)
  fe <- liftIO (doesFileExist fn)
  when fe rv

readCda:: BS.ByteString -> Either XMLParseError (UNode T.Text)
readCda text = parse' defaultParseOptions text

parseCda:: BS.ByteString -> BS.ByteString
parseCda inputText = 
  let 
    p = readCda inputText
    l = fromIntegral (BS.length inputText) :: Int64
  in
  if l > maxMb then "/toolong" else
    case p of
      Right cda -> "/document"
      Left err -> "/error"
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

