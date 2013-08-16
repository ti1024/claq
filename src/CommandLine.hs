{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module CommandLine (Report(..), CommandLine(..), parseOptions, parseCommandLine) where

import Control.Monad (foldM, unless, when)
import Control.Monad.Instances () -- To import instance Monad (Either e)
import System.Console.GetOpt
import System.Environment (getProgName, getArgs)
import System.Exit
import System.IO

data Report = ReportCircuitToffoli | ReportCircuitStd | ReportRawToffoli
  deriving (Eq, Show)

parseReport :: String -> Maybe Report
parseReport "circuit" = Just ReportCircuitToffoli
parseReport "circuitstd" = Just ReportCircuitStd
parseReport "raw" = Just ReportRawToffoli
parseReport _ = Nothing

data CommandLine = CommandLine {
  cmdFilenames :: [FilePath],
  cmdReport :: Report,
  cmdCoherent :: Bool
}

data Options = Options {
  optFilenames :: [FilePath],
  optReport :: Report,
  optCoherent :: Bool,
  optHelp :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
  optFilenames = [],
  optReport = ReportCircuitToffoli,
  optCoherent = True,
  optHelp = False
}

parseOptions :: String -> [String] -> Either (String, Bool) CommandLine
parseOptions progName args = do
    let header = "Usage: " ++ progName ++ " [option...] input..."
    let (o, nonopts, errs) = getOpt Permute options args
    unless (null errs) $
      Left (concat errs ++ "\n" ++ usageInfo header options, True)
    opts <- foldM (flip id) defaultOptions (o ++ map parseOptFilename nonopts)
    when (optHelp opts) $
      Left (usageInfo header options, False)
    when (null (optFilenames opts)) $
      Left ("no input files\n\n" ++ usageInfo header options, True)
    return CommandLine {
        cmdFilenames = reverse $ optFilenames opts,
        cmdReport = optReport opts,
        cmdCoherent = optCoherent opts
      }
  where
    options = [
        Option [] ["report"] (ReqArg parseOptReport "TYPE") "type of report: circuit (default), circuitstd, raw",
        Option ['s'] [] (NoArg parseOptReportStd) "same as --report=circuitstd",
        Option ['n'] ["noncoherent"] (NoArg parseOptNoncoherent) "implement classical function noncoherently",
        Option ['h'] ["help"] (NoArg parseOptHelp) "show this message and exit"
      ]
    parseOptFilename arg opts =
        Right $ opts { optFilenames = arg : optFilenames opts }
    parseOptReport arg opts =
        case parseReport arg of
          Just report -> Right $ opts { optReport = report }
          Nothing -> Left ("unrecognized report type `" ++ arg ++ "\'", True)
    parseOptReportStd opts =
        Right $ opts { optReport = ReportCircuitStd }
    parseOptNoncoherent opts =
        Right $ opts { optCoherent = False }
    parseOptHelp opts =
        Right $ opts { optHelp = True }

parseCommandLine :: IO CommandLine
parseCommandLine = do
    progName <- getProgName
    args <- getArgs
    case parseOptions progName args of
      Right res -> return res
      Left (msg, False) -> do
        putStr msg
        exitSuccess
      Left (msg, True) -> do
        hPutStr stderr msg
        exitFailure
