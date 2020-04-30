{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main (
    main
) where

import Data.Semigroup
import Data.Text (Text)
import System.IO.Temp

import System.Linux.Namespaces
import System.Posix
import System.Process
import Control.Monad.Catch

import Test.DocTest
import System.IO (stderr, BufferMode(..), stdout, hSetBuffering)
import System.FilePath ((</>))
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (readProcess, system)
import Control.Concurrent
       (takeMVar, newEmptyMVar, forkIO, threadDelay, putMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Test.WebDriver (runSession, defaultConfig, openPage, closeSession)
import Test.WebDriver.Class (WebDriver)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

seleniumPort, jsaddlePort :: Int
seleniumPort = 8000
jsaddlePort = 8001

chromeConfig :: Text -> [Text] -> WD.WDConfig
chromeConfig fp flags = WD.useBrowser (WD.chrome { WD.chromeBinary = Just $ T.unpack fp, WD.chromeOptions = T.unpack <$> flags }) WD.defaultConfig

main :: IO ()
main = do
    -- unshareNetork
    putStrLn "Testing JSaddle"
    jsaddlePath <- getArgs >>= \case
        [arg] -> return arg
        _ -> do
            putStrLn "Please give the path to the jsaddle package source"
            exitFailure
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- isHeadless <- (== Nothing) <$> lookupEnv "NO_HEADLESS"
    let isHeadless = False
    withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
      withSeleniumServer $ \selenium -> do
        let browserPath = "chromium"
        when (T.null browserPath) $ fail "No browser found"
        -- withDebugging <- isNothing <$> lookupEnv "NO_DEBUG"
        let wdConfig = WD.useBrowser WD.chrome $ WD.defaultConfig { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
            chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
            -- session' = sessionWith wdConfig "" . using (map (,"") caps)
        runSession wdConfig (tests jsaddlePath) `finally` _selenium_stopServer selenium
  where
  tests :: (WebDriver m, MonadIO m) => String -> m ()
  tests jsaddlePath = do
    liftIO $ putStrLn "Running Tests"
    done <-  liftIO $ newEmptyMVar
    liftIO $ forkIO $ do
        doctest [
            "-hide-all-packages",
            "-package=base-" ++ VERSION_base,
            "-package=lens-" ++ VERSION_lens,
            "-package=text-" ++ VERSION_text,
            "-package=bytestring-" ++ VERSION_bytestring,
            "-package=transformers-" ++ VERSION_transformers,
            "-package=websockets-" ++ VERSION_websockets,
            "-package=primitive-" ++ VERSION_primitive,
            "-package=aeson-" ++ VERSION_aeson,
            "-package=websockets-" ++ VERSION_websockets,
            "-package=wai-" ++ VERSION_wai,
            "-package=wai-websockets-" ++ VERSION_wai_websockets,
            "-package=warp-" ++ VERSION_warp,
            "-package=http-types-" ++ VERSION_http_types,
            "-package=stm-" ++ VERSION_stm,
            "-package=time-" ++ VERSION_time,
            "-package=containers-" ++ VERSION_containers,
            "-package=process-" ++ VERSION_process,
            "-package=filepath-" ++ VERSION_filepath,
            "-package=ref-tf-" ++ VERSION_ref_tf,
            "-package=deepseq-" ++ VERSION_deepseq,
            "-package=ghc-prim-" ++ VERSION_ghc_prim,
            "-package=exceptions-" ++ VERSION_exceptions,
            "-package=unliftio-core-" ++ VERSION_unliftio_core,
            "-package=random-" ++ VERSION_random,
            "-package=foreign-store-" ++ VERSION_foreign_store,
            "-i" <> "src",
            "-i" <> "src-ghc",
            "src/Language/Javascript/JSaddle/Test.hs",
            "-i" <> jsaddlePath,
            jsaddlePath </> "src-ghc/Data/JSString/Text.hs",
            jsaddlePath </> "src-ghc/Data/JSString/Internal/Type.hs",
            jsaddlePath </> "src-ghc/GHCJS/Foreign.hs",
            jsaddlePath </> "src-ghc/GHCJS/Foreign/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Internal/Types.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal/Pure.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Prim.hs",
            jsaddlePath </> "src-ghc/GHCJS/Prim/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Types.hs",
            jsaddlePath </> "src-ghc/JavaScript/Array.hs",
            jsaddlePath </> "src-ghc/JavaScript/Array/Internal.hs",
            jsaddlePath </> "src-ghc/JavaScript/Object/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Arguments.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Classes.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Classes/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Debug.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Evaluate.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Exception.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Monad.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Marshal/String.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Native.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Native/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Object.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Properties.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Run.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Run/Files.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/String.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Types.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Value.hs" ]
        putMVar done ()
    liftIO $ threadDelay 5000000
    openPage "http://127.0.0.1:3709"
    liftIO $ takeMVar done
    closeSession

withSandboxedChromeFlags :: Bool -> ([Text] -> IO a) -> IO a
withSandboxedChromeFlags headless action =
  withSystemTempDirectory "reflex-dom-core_test" $ \tmp ->
    action
      [ if headless then "--headless" else "--auto-open-devtools-for-tabs"
      , "--disable-gpu"
      , "--no-sandbox"
      , "--remote-debugging-port=9222"
      , "--user-data-dir=" <> T.pack tmp
      ]

unshareNetork :: IO ()
unshareNetork = do
  uid <- getEffectiveUserID
  unshare [User, Network]
  writeUserMappings Nothing [UserMapping 0 uid 1]
  callCommand "ip link set lo up ; ip addr"

data Selenium = Selenium
  { _selenium_portNumber :: Int
  , _selenium_stopServer :: IO ()
  }

startSeleniumServer :: Int -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  return $ terminateProcess ph

withSeleniumServer :: (Selenium -> IO ()) -> IO ()
withSeleniumServer f = do
  stopServer <- startSeleniumServer seleniumPort
  threadDelay $ 1000 * 1000 * 2 -- TODO poll or wait on a a signal to block on
  f $ Selenium
    { _selenium_portNumber = seleniumPort
    , _selenium_stopServer = stopServer
    }
