{-# LANGUAGE OverloadedStrings #-}

module Cda.Splises.Compiled
  ( 
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
-- import           Snap.Snaplet.Session.Backends.CookieSession
-- import           Snap.Util.FileServe
import           Heist
import qualified Heist.Compiled as C
-- import qualified Heist.Compiled.LowLevel as LL
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

parseResult :: Monad m => T.Text -> RuntimeSplice m T.Text
parseResult text = return text       

spResult :: Monad m => Splice m
spResult = return $ C.yieldPureText $ parseResult

resultSplices :: Monad m => Splices (Splice m)
resultSplices = do
    "result" #! spResult

