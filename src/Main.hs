module Main where

import Frontend
import Backend


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

import qualified Data.Text as T
import qualified Data.Map as Map


-- runStateM :: StateM s a -> s -> IO (Either T.Text a)
-- runStateM comp s = evalStateT (runExceptT comp) s
runStateM :: StateM s a -> s -> ExceptT T.Text IO a
runStateM comp s = evalStateT comp s

isError :: Either T.Text b -> Bool
isError (Left _) = True
isError _ = False

writeOutput :: Either T.Text a -> FilePath -> IO ()
writeOutput res fp = case res of
  Left err -> writeFile fp $ "ERROR\nSEMANTIC CHECK FAILED:\n" ++ (T.unpack err)
  Right sth -> writeFile fp $ "OK\n"

run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  let outFile = dropExtension fp <.> "myout"
  case ts of
           Bad s    -> do
             writeFile outFile "ERROR\n"
           Ok  tree -> do
             res <- runExceptT $ checkAll tree
             writeOutput res outFile

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f


main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _  -> putStrLn "no filename argument supplied!"
