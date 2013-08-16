{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (forM_, toList, traverse_)
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (traverse)
import qualified System.Exit as Exit
import qualified System.IO as IO
import Text.Parsec.ByteString.Lazy

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.CircuitBuilder
import Control.Monad.Quantum.CircuitBuilderToffoli
import Data.Quantum.Circuit (Wire, Gate, Circuit, injectCircuit)
import Data.Quantum.Circuit.QC (runQCBuilder)

import CommandLine
import Compile
import Data.ClassicalCircuit
import qualified Language.Claq.Parser as Parser
import qualified Language.Claq.Syntax as Syntax

main :: IO ()
main = do
    cmdline <- parseCommandLine
    forM_ (cmdFilenames cmdline) $ \filename -> do
      parsedCircuit <- parseFromFile Parser.circuit filename
      case parsedCircuit of
        Left err -> do
          IO.hPrint IO.stderr err
          Exit.exitFailure
        Right stmts ->
          case Syntax.stmtsToCircuit stmts of
            Left err -> do
              IO.hPutStrLn IO.stderr err
              Exit.exitFailure
            Right (clacir, innames, os, _wireNameToIndexMap) -> do
              let builder :: MonadQuantum Wire m => m ([(String, Wire)], [Wire])
                  builder = do
                    inputs <- traverse (\inname -> newWire >>= \w -> return (inname, w)) innames
                    let innameToWireMap = Map.fromList inputs
                        innameToWire inname = Map.findWithDefault undefined inname innameToWireMap
                    outws <- toList <$> prepareExpr (bit . innameToWire <$> clacir) (Seq.fromList os)
                    return (inputs, outws)
                  prepareExpr :: MonadQuantum Wire m => ClaCircuit (Bit Wire) -> Seq Int -> m (Seq Wire)
                  prepareExpr =
                    if cmdCoherent cmdline then
                      prepareExprCoherent
                    else
                      prepareExprNoncoherent
              case cmdReport cmdline of
                ReportRawToffoli -> do
                  let circuit :: Circuit Gate Wire
                      ((inputs, outws), circuit) = evalCircuitBuilderToffoli builder [] 0
                  reportInputsOutputs inputs outws
                  print circuit
                ReportCircuitToffoli -> do
                  let circuit :: Circuit Gate Wire
                      ((inputs, outws), circuit) = evalCircuitBuilderToffoli builder [] 0
                  reportInputsOutputs inputs outws
                  ByteString.putStr $ runQCBuilder IO.nativeNewline $ injectCircuit circuit
                ReportCircuitStd -> do
                  let circuit :: Circuit Gate Wire
                      ((inputs, outws), circuit) = evalCircuitBuilder builder [] 0
                  reportInputsOutputs inputs outws
                  ByteString.putStr $ runQCBuilder IO.nativeNewline $ injectCircuit circuit
  where
    reportInputsOutputs inputs outws = do
        putStrLn "# Inputs"
        traverse_ (\(name, w) -> putStrLn $ "#   " ++ name ++ " " ++ show w) inputs
        putStrLn "# Outputs"
        traverse_ (\w -> putStrLn $ "#   " ++ show w) outws
