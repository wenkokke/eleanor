{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified STLC.Duregård as STLC
import qualified LLAM.Duregård as LLAM

import Control.Search (search)
import Data.Coolean (toBool)
import Data.Version (showVersion)
import Data.Proxy (Proxy(..))
import Paths_eleanor (version)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, stdout)
import Test.Feat.Access (valuesWith)
import Text.Read (readMaybe)

data System
  = Untyped
  | SimplyTyped
  | Linear
  deriving (Read, Show, Enum, Bounded)

data Action
  = Count
  | Print
  deriving (Read, Show, Enum, Bounded)

data Strategy
  = Feat
  | Neat
  deriving (Read, Show, Enum, Bounded)

data Options
  = Options
    { optSystem   :: System
    , optSize     :: Int
    , optAction   :: Action
    , optStrategy :: Strategy
    }

defaultOptions :: Options
defaultOptions
  = Options
    { optSystem   = Untyped
    , optSize     = 30
    , optAction   = Count
    , optStrategy = Neat
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['y'] ["system"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optSystem = val }))
        "SYSTEM")
      $ "Any type system from " ++ showEnum (Proxy :: Proxy System)   ++ "."
    , Option ['a'] ["action"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optAction = val }))
        "ACTION")
      $ "Any action from "      ++ showEnum (Proxy :: Proxy Action)   ++ "."
    , Option ['t'] ["strategy"]
      (ReqArg ((\arg opts -> do val <- readError arg; return opts { optStrategy = val }))
        "STRATEGY")
      $ "Any strategy from "    ++ showEnum (Proxy :: Proxy Strategy) ++ "."
    , Option ['i'] ["size"]
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
                 prg <- getProgName
                 hPutStrLn stdout $ prg ++ " " ++ showVersion version
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
  let Options { optSystem   = sys
              , optSize     = size
              , optAction   = action
              , optStrategy = strategy
              } = opts


  let untyped         = concatMap snd . take size . valuesWith $ STLC.closed
  let sampleType      = STLC.Void STLC.:-> STLC.Void
  let simplyTypedFeat = filter (toBool . STLC.checkClosed sampleType) untyped
  let simplyTypedNeat = search size (STLC.checkClosed sampleType)
  let linearFeat      = filter (LLAM.checkClosed sampleType) untyped
  let linearNeat      = search size (LLAM.checkClosed sampleType)

  case sys of
    Untyped ->
      case action of
        Print -> print untyped
        Count ->
          case strategy of
            Feat -> print . length $ untyped
            Neat -> print . sum . map fst . take size . valuesWith $ STLC.closed
    SimplyTyped ->
      case strategy of
        Feat ->
          case action of
            Print -> print simplyTypedFeat
            Count -> print . length $ simplyTypedFeat
        Neat ->
          case action of
            Print -> print =<< simplyTypedNeat
            Count -> print . length =<< simplyTypedNeat
    Linear ->
      case strategy of
        Feat ->
          case action of
            Print -> print linearFeat
            Count -> print . length $ linearFeat
        Neat ->
          case action of
            Print -> print =<< linearNeat
            Count -> print . length =<< linearNeat


