{-# LANGUAGE OverloadedStrings #-}

module Cda.Handlers where

-- import qualified Snap.Core as S
import  Control.Monad.IO.Class as LIO
import  Snap.Core
import  Snap.Util.FileUploads
import  Snap.Snaplet
import  Snap.Snaplet.Heist
import  Snap.
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


import  Application


------------------------------------------------------------------------

cdaBase :: AppHandler ()
cdaBase = render "index"

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

cdaUploadHandler :: MonadSnap m => 
                [(PartInfo, Either PolicyViolationException FilePath)] -> m ()
cdaUploadHandler xs = 
  mapM_ handleOne (filter wanted xs) where
    wanted (_ , Left _) = False
 	  wanted (_ , Right _) = True
 	  handleOne (partInfo, Right filepath) = cdaRender partInfo filepath     

tempDir:: ByteString
tempDir = "tmp"

cdaHandleFileUploads :: MonadSnap m => FilePath -> m ()
cdaHandleFileUploads tempDir = 
   handleFileUploads tempDir cdaDefaultPolicy cdaPerPartPolicy cdaUploadHandler

cdaRender:: PartInfo -> FilePath -> AppHandler ()
cdaRender partInfo file = do
  inputText <- LIO.liftIO $ L.readFile filename
  let (xml, mErr) = parse defaultParseOptions inputText :: (UNode T.Text, Maybe XMLParseError)
  case mErr of
    Nothing ->  do
    Just err -> do
      hPutStrLn stderr $ "XML parse failed: "++show err
      exitWith $ ExitFailure 2






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

