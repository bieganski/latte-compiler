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


isError :: Either T.Text b -> Bool
isError (Left _) = True
isError _ = False

writeOutput :: Either T.Text a -> FilePath -> IO ()
writeOutput res fp = case res of
  Left err -> writeFile fp $ "ERROR\nSEMANTIC CHECK FAILED:\n" ++ (T.unpack err)
  Right sth -> return () -- writeFile outFile $ "OK\n"


doTest :: [Either T.Text a] -> FilePath -> IO ()
doTest lst fp = case True `elem` (map isError lst) of
  False -> writeFile fp $ "OK\n"
  True -> forM_ lst (flip writeOutput fp)


run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  let outFile = dropExtension fp <.> "myout"
  case ts of
           Bad s    -> do
             writeFile outFile "ERROR\n"
           Ok  tree -> do
             resFunctions <- runStateM (checkFunctionCases tree) funCheckState0
             doTest [resFunctions] outFile
             resReturn <- runExceptT (returnsProperly tree)
             resArgs <- runExceptT (uniqueArgs tree)
             resUniqueVars <- runReaderT (runExceptT $ uniqueVars tree) []
             resDeclaredVars <- runReaderT (runExceptT $ onlyDeclaredVarsUsed tree) []
             resProperCallNum <- runStateM (properArgumentNumberCalls tree) []
             typeCheck <- runReaderT (runExceptT $ typeCheck tree) (FunName (Ident "dummy"), Map.empty, Map.empty)
             let res = [resFunctions,
                        resReturn,
                        resArgs,
                        resUniqueVars,
                        resDeclaredVars,
                        resProperCallNum,
                        typeCheck]
             doTest res outFile
{-
             writeOutput resFunctions outFile
             case resFunctions of
               Left s -> do
                 putStrLn $ T.unpack s
                 writeFile outFile $ "ERROR\nSEMANTIC CHECK FAILED:\n" ++ (T.unpack s)
               Right _ -> do
                 writeFile outFile "OK\n"
-}

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f


main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _  -> putStrLn "auu test"  
    -- _ -> error"args error!"

