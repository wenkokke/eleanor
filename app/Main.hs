{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified STLC.Duregård as STLC
import qualified LLAM.Duregård as LLAM
import Text.Read (readMaybe)
import Data.Version (showVersion)
import Data.Proxy (Proxy(..))
import Paths_eleanor (version)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, stdout)
import Control.Search (search)
import Test.Feat.Access (valuesWith)

data System
  = Untyped
  | SimplyTyped
  | Linear
  deriving (Read, Show, Enum, Bounded)

data Action
  = Count
  | Print
  deriving (Read, Show, Enum, Bounded)

data Options
  = Options
    { optSystem :: System
    , optSize   :: Int
    , optAction :: Action
    }

defaultOptions
  = Options
    { optSystem = Untyped
    , optSize   = 30
    , optAction = Count
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option [] ["system"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optSystem = val }))
        "SYSTEM")
      $ "Any type system from " ++ showEnum (Proxy :: Proxy System) ++ "."
    , Option [] ["action"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optAction = val }))
        "ACTION")
      $ "Any action from " ++ showEnum (Proxy :: Proxy Action) ++ "."
    , Option [] ["size"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optSize = val }))
        "SIZE")
        "The maximum term size."
    , Option "h" ["help"]
      (NoArg  (\_ -> do
                  prg <- getProgName
                  hPutStrLn stderr (usageInfo prg options)
                  exitSuccess))
      "Show help."
    , Option "v" ["version"]
      (NoArg (\_ -> do
                 hPutStrLn stdout $ "agda2html " ++ showVersion version
                 exitSuccess))
      "Show version."
    ]

showEnum :: (Show a, Enum a, Bounded a) => Proxy a -> String
showEnum (Proxy :: Proxy a) = show [minBound .. maxBound :: a]

readError :: Read a => String -> IO a
readError str = case readMaybe str of
  Just val -> return val
  Nothing  -> do putStrLn $ "Error: unknown value '" ++ str ++ "'"
                 exitFailure

main :: IO ()
main = do
  (actions, _, _) <- getOpt Permute options <$> getArgs

  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optSystem = sys
              , optSize   = size
              , optAction = action
              } = opts

  let sampleType = STLC.Void STLC.:-> STLC.Void

  case sys of
    Untyped -> do
      case action of
        Print -> do
          print . concatMap snd . take size . valuesWith $ STLC.closed
        Count -> do
          print . sum . map fst . take size . valuesWith $ STLC.closed
    SimplyTyped -> do
      case action of
        Print -> do
          print =<< search size (STLC.checkClosed sampleType)
        Count -> do
          print . length =<< search size (STLC.checkClosed sampleType)
    Linear -> do
      case action of
        Print -> do
          print =<< search size (LLAM.checkClosed sampleType)
        Count -> do
          print . length =<< search size (LLAM.checkClosed sampleType)

