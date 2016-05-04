{-# LANGUAGE OverloadedStrings #-}

module Cda.Handlers where

-- import qualified Snap.Core as S
import  Control.Monad.IO.Class as LIO
import  Snap.Core
import  Snap
import  Snap.Util.FileUploads
import  Snap.Snaplet
import  Snap.Snaplet.Heist
import  Snap.Iteratee (consume)
-- import  Snap.Snaplet.Session
import  qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as S
-- import  qualified Data.String.UTF8 as T8
-- import  Data.String.Conversions 
-- import  Data.Maybe ( fromJust )
-- import  qualified Data.Text as T
-- import  Heist
-- import  Data.Monoid
-- import  qualified Heist.Interpreted as I
-- import  qualified Heist.Compiled as C
-- import  qualified  Text.XmlHtml as X

-- import  Control.Applicative
-- import  Text.Digestive
-- import  Text.Digestive.Heist
-- import  qualified Text.Digestive.Snap as DS

-- import  qualified Data.Aeson as A

-- import System.Directory
-- getTemporaryDirectory :: IO FilePath
import  qualified Data.Text as T
import  Text.XML.Expat.Tree
-- import Text.XML.Expat.Format
import  Text.XML.Expat.Proc
-- import System.Environment
-- import System.Exit
import  System.IO
import  qualified Data.ByteString.Lazy as L
import  Data.Maybe ( fromJust, maybe )
-- import Control.Monad 
-- import Data.Monoid
-- import qualified Text.XML.Expat.Lens.Unqualified as XL
-- import Control.Lens 
-- import Data.Default (def)

import  Heist
import  qualified Heist.Interpreted as I
-- import  qualified Heist.Compiled as C
-- import  qualified Heist.Compiled.LowLevel as LL
import  Application


------------------------------------------------------------------------

cdaBase :: AppHandler ()
cdaBase = render "index"


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

cdaUploadHandler :: (Monad m, MonadIO m) =>
                [(PartInfo, Either PolicyViolationException FilePath)] -> 
                  m (Splices (SnapletISplice App))

--                m (Maybe T.Text)
cdaUploadHandler xs = do
  let
    ff = "files" ## (files xs)
    files = I.mapSplices $ I.runChildrenWith . fromEt
    fromEt (p, e) = "file" ## I.textSplice $ T.pack $ show p ++ " " ++ show e 
  return ff      

{-  
  handleOne $ head (filter wanted xs)
  where 
    wanted (_ , Right _) = True
    wanted (_ , Left _) = False
    handleOne (partInfo, Right filepath) = cdaRender partInfo filepath
-}

cdaRender:: (Monad m, MonadIO m) => PartInfo -> FilePath -> m (Maybe T.Text)
cdaRender partInfo filepath = do
  inputText <- LIO.liftIO $ L.readFile filepath
  let (xml, mErr) = parse defaultParseOptions inputText :: (UNode T.Text, Maybe XMLParseError)
  case mErr of
    Nothing -> return $ Just "Success parsed"
    Just err -> return $ Just $ T.pack $ "XML parse failed: "++ show err 

-- createDirectoryIfMissing:: Bool -> FilePath -> IO () and 
-- getAppUserDataDirectory :: String -> IO FilePath.
    
uploadHandler :: AppHandler ()
uploadHandler = method POST $ do
  -- :: Maybe Text
  s <- handleFileUploads tempDir thisUpl cdaPerPartPolicy cdaUploadHandler
  renderWithSplices "upload" s
{-  
  let
    t = I.textSplice $ fromJust s
  heistLocal (I.bindSplice "result" t) $ render "upload"
-}
uploadFiles :: AppHandler ()
uploadFiles = do
  files <- handleMultipart thisUpl $ \part -> do
    content <-  liftM BS.concat consume
    return (part, content)
  let r = if null files then "No files uploaded" else parseCda (head files)

  renderWithSplices "upload" ("result" ## I.textSplice r)


parseCda:: (PartInfo, BS.ByteString) -> T.Text
parseCda (partInfo, inputText) = 
  let 
    p = parse' defaultParseOptions inputText :: Either XMLParseError (UNode T.Text)
  in
  case p of
    Right _ -> "File successfully parsed"
    Left err -> T.pack $ "XML parse failed: "++ show err 



  -- liftIO $ putStrLn $ "cont " ++ show (length files)
  -- s <- viewF files
{-  
  if any (\(_,c) -> (BS.length c) > 2000000) files
    then error "One of the uploaded files is bigger then 2Mb limit size!"
    else renderWithSplices "upoad" s
-}
  -- renderWithSplices "upload" (viewF files)
{-
viewF:: (Monad m, MonadIO m) => [(PartInfo, BS.ByteString)] -> m (Splices (SnapletISplice App))
viewF fs = do
  let
    ff = "files" ## (files fs)
    files = I.mapSplices $ I.runChildrenWith . fromEt
    fromEt (p, c) = "file" ## I.textSplice $ T.pack $ "herr"
  return ff
-}

viewF:: [(PartInfo, BS.ByteString)] -> (Splices (SnapletISplice App))
viewF fs = "files" ## (files fs)
  where
    files = I.mapSplices $ I.runChildrenWith . fromEt
    fromEt (p, c) = "file" ## I.textSplice $ T.pack $ show c



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

