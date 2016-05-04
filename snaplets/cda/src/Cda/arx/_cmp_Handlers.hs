{-# LANGUAGE OverloadedStrings #-}

module Cda.Handlers where

-- import qualified Snap.Core as S
import  Control.Monad.IO.Class as LIO
import  Snap.Core
import  Snap
import  Snap.Util.FileUploads
import  Snap.Snaplet
import  Snap.Snaplet.Heist

-- import  Snap.Snaplet.Session
-- import  qualified Data.ByteString as BS
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
import  qualified Heist.Compiled as C
import  qualified Heist.Compiled.LowLevel as LL
import  Application


------------------------------------------------------------------------

cdaBase :: AppHandler ()
cdaBase = cRender "index"

uploadHandler :: Handler App App ()
uploadHandler = method GET $ cRender "upload"



maxMb = 2
megaByte = 2^(20::Int)

cdaDefaultPolicy :: UploadPolicy
cdaDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy

cdaPerPartPolicy :: PartInfo -> PartUploadPolicy
cdaPerPartPolicy _ = allowWithMaximumSize (maxMb * megaByte)
{-
data PartInfo =
    PartInfo { partFieldName   :: !ByteString
             , partFileName    :: !(Maybe ByteString)
             , partContentType :: !ByteString
             }
-}

-- createDirectoryIfMissing:: Bool -> FilePath -> IO () and 
-- getAppUserDataDirectory :: String -> IO FilePath.
tempDir::String
tempDir = "tmp"

cdaUploadHandler :: (Monad m, MonadIO m) =>
                [(PartInfo, Either PolicyViolationException FilePath)] -> m (Maybe T.Text)
cdaUploadHandler xs =
  handleOne $ head (filter wanted xs)
  where 
    wanted (_ , Right _) = True
    wanted (_ , Left _) = False
    handleOne (partInfo, Right filepath) = cdaRender partInfo filepath
    
cdaRender:: (Monad m, MonadIO m) => PartInfo -> FilePath -> m (Maybe T.Text)
cdaRender partInfo filepath = do
  inputText <- LIO.liftIO $ L.readFile filepath
  let (xml, mErr) = parse defaultParseOptions inputText :: (UNode T.Text, Maybe XMLParseError)
  case mErr of
    Nothing -> return Nothing
    Just err -> return $ Just $ T.pack $ "XML parse failed: "++ show err 

runtimeUpload :: (Monad m, MonadSnap m, MonadIO m) => RuntimeSplice m (Maybe T.Text)
runtimeUpload = do  
  s <- lift $ handleFileUploads tempDir cdaDefaultPolicy cdaPerPartPolicy cdaUploadHandler
  return s

uplRTSplices :: (Monad m, MonadSnap m) => Splices (C.Splice m)
uplRTSplices = do
  "upload" ## runtimeValue runtimeUpload
--  "runtimeB" ## runtimeValue runtimeB

runtimeValue :: Monad m => RuntimeSplice m (Maybe T.Text) -> C.Splice m
runtimeValue runtime = do
  -- a compiled splice for a static template (will be used in the Nothing case)
  nothing     <- C.callTemplate "success"
  promise     <- LL.newEmptyPromise
  valueSplice <- getValueSplice (LL.getPromise promise)
-- The 'do' block below has a value of type:
--   RuntimeSplice n Builder -> DList (Chunk n)
  let builder = C.yieldRuntime $ do
        value <- runtime
        case value of
          -- in the Nothing case we convert the template splice to a builder
          Nothing -> C.codeGen nothing
          -- in the Just case we put the extracted Text value in a promise
          -- first, then convert the compiled value splice to a builder
          Just v  -> do
            LL.putPromise promise v
            C.codeGen valueSplice

  -- our builder has the type DList (Chunk n), and remember that:
  --   type Splice n = HeistT n IO (DList (Chunk n))
  -- so returning this value to the current monad finally creates the fully
  -- compiled splice we needed
  return builder

getValueSplice :: Monad m => RuntimeSplice m T.Text -> C.Splice m
getValueSplice = C.withSplices template local
  where 
    template = C.callTemplate "parser_error"
    local    = "error" ## C.pureSplice . C.textSplice $ id



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

