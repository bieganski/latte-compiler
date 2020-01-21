module Main where

import Frontend
import Backend(runBackend)


import AbsLatte
import Types
import ParLatte
import SkelLatte
import PrintLatte
import SkelLatte
import LexLatte
import ErrM

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Exit

import qualified Data.Text as T
import qualified Data.Map as Map

type Verbosity = Int
myLLexer = myLexer

isError :: Either T.Text b -> Bool
isError (Left _) = True
isError _ = False

run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  let outFile = dropExtension fp <.> "ll"
  case ts of
           Bad s    -> do
             hPutStrLn stderr "ERROR"
             die s
           Ok  tree -> do
             res <- runExceptT $ checkAll tree
             case res of
               Left t -> do
                 hPutStrLn stderr $ "ERROR"
                 die $ "frontend check failed:\n" ++ T.unpack t
               Right newTree -> do
                 let res2 = runBackend fp newTree
                 case res2 of
                   Right t -> do
                     hPutStrLn stderr $ "OK"
                     writeFile outFile $ T.unpack t
                   Left t -> do
                     hPutStrLn stderr $ "ERROR"
                     die $ T.unpack t
                 return () 
                 

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f


main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _  -> putStrLn "no filename argument supplied!"
