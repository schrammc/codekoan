{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
module Thesis.Util.LoggingUtils where

import Control.Monad.Logger

import System.IO
import Data.Time.Clock
import Data.Monoid ((<>))
import Data.ByteString.Char8 as BS
import System.Log.FastLogger (fromLogStr)

runOutstreamLogging :: LoggingT m a -> m a
runOutstreamLogging  = (`runLoggingT` writeOutput)
  where
    writeOutput location source level str = do
      let handle = getHandle level
      formatted <- formatStr location source level str
      BS.hPutStrLn handle (fromLogStr formatted)
      hFlush handle

    getHandle LevelError = stderr
    getHandle LevelWarn  = stderr
    getHandle _ = stdout

    formatStr location _ level str = do
      time <- getCurrentTime
      return $ (toLogStr $ show time) <> " -- " <> formatLevel level
               <> " :: " <> str <> " <<Source: "
               <> (toLogStr $ loc_package location) <> "/"
               <> (toLogStr $ loc_module location) <> " " <>
               (toLogStr $ (show $ loc_start location)) <> ">>" 

    formatLevel :: LogLevel -> LogStr
    formatLevel (LevelOther lvl) = "[" <> (toLogStr lvl) <> "]"
    formatLevel (LevelDebug) = "[Debug]"
    formatLevel (LevelInfo)  = "[Info]"
    formatLevel (LevelWarn)  = "[Warn]"
    formatLevel (LevelError) = "[Error]"


testLogging :: LoggingT IO ()
testLogging = do
  $(logDebug) "debugging"
  $(logError) "error"
