{-# LANGUAGE PackageImports        #-}

module Language.PureScript.Ide.Logging
       ( runLogger
       , runLogger'
       , runLoggerWithFile
       , logPerf
       , displayTimeSpec
       , labelTimespec
       ) where

import Protolude hiding (handle)

import "monad-logger" Control.Monad.Logger (LogLevel(..), LoggingT, MonadLogger, filterLogger, logOtherN, runStdoutLoggingT)
import "monad-logger" Control.Monad.Logger qualified as L
import Data.Text qualified as T
import Language.PureScript.Ide.Types (IdeLogLevel(..))
import System.Clock (Clock(..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import Text.Printf (printf)
import GHC.IO.Handle (hFlush)
import Data.Text.IO qualified as TIO
import Data.Text.Encoding qualified as TE

logToStdoutAndFile :: Maybe IdeLogLevel -> Maybe (IdeLogLevel, MVar Handle) -> L.Loc -> L.LogSource -> LogLevel -> L.LogStr -> IO ()
logToStdoutAndFile stdLevel fileLevel loc source level msg = do
    let logLine = L.defaultLogStr loc source level msg
    let lineText = TE.decodeUtf8 (L.fromLogStr logLine)

    case levelFilter level <$> stdLevel   of
      Just True ->
        TIO.hPutStr stdout lineText
      _ -> pure ()

    case fmap (levelFilter level) . swap <$> fileLevel of
      Just (handleVar, True) ->
        withMVar handleVar $ \handle -> do
          TIO.hPutStr handle lineText
          hFlush handle
      _ -> pure ()

levelFilter :: LogLevel -> IdeLogLevel -> Bool
levelFilter logLevel =
  \case
    LogAll -> True
    LogDefault -> not (logLevel == LevelOther "Perf" || logLevel == LevelDebug)
    LogNone -> False
    LogDebug -> logLevel /= LevelOther "Perf"
    LogPerf -> logLevel /= LevelDebug

runLogger :: MonadIO m => IdeLogLevel -> LoggingT m a -> m a
runLogger ideLogLevel =
  runStdoutLoggingT . filterLogger (\_ -> flip levelFilter ideLogLevel)

-- First log level for stdout, second for file.
runLogger' :: Maybe IdeLogLevel -> Maybe (IdeLogLevel, MVar Handle) -> LoggingT m a -> m a
runLogger' stdLevel fileLevel =
  flip L.runLoggingT (logToStdoutAndFile stdLevel fileLevel)

runLoggerWithFile :: IdeLogLevel -> MVar Handle -> LoggingT m a -> m a
runLoggerWithFile ll handleVar =
  runLogger' (Just ll) (Just (ll, handleVar))



labelTimespec :: Text -> TimeSpec -> Text
labelTimespec label duration = label <> ": " <> displayTimeSpec duration

logPerf :: (MonadIO m, MonadLogger m) => (TimeSpec -> Text) -> m t -> m t
logPerf format f = do
  start <- liftIO (getTime Monotonic)
  result <- f
  end <- liftIO (getTime Monotonic)
  logOtherN (LevelOther "Perf") (format (diffTimeSpec start end))
  pure result

displayTimeSpec :: TimeSpec -> Text
displayTimeSpec ts =
  T.pack (printf "%0.2f" (fromIntegral (toNanoSecs ts) / 1000000 :: Double)) <> "ms"
