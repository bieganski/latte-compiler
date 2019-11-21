{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import AbsLatte
import ParLatte
import SkelLatte
import PrintLatte
import SkelLatte
import LexLatte
import ErrM

import System.IO (hGetContents)
import System.Environment (getArgs)
import qualified Data.Text as T
import Text.Printf(printf)
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad(forM)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import System.FilePath
import System.Process
import qualified Data.Set as S
import Data.List.Unique(repeated)
import Data.List(elemIndex)

-- bnfc stuff
type ParseFun a = [Token] -> Err a
myLLexer = myLexer
type Verbosity = Int



type EnvM e   = ExceptT T.Text (ReaderT e IO)
type StateM s = ExceptT T.Text (StateT s IO)

a :: EnvM Integer Integer
a = undefined

type FunctionCheckM = StateM (S.Set String)

getFunctionNames :: Program -> [String]
getFunctionNames (Program topDefs) = map (\(FnDef _ (Ident name) _ _ ) -> name) topDefs

checkFunctionNamesUnique :: Program -> S.Set String -> Bool
checkFunctionNamesUnique p@(Program topDefs) s = (length topDefs) == (S.size s)

  {-
getFunctionNamesWithPreCheck :: Program -> S.Set String
getFunctionNamesWithPreCheck p@(Program topDefs) =
  if checkFunctionNamesUnique p names then names else error ""
  -}

type FunctionM = StateM (S.Set String)


-- err :: Monad a => forall (a :: * -> *) b. String -> ExceptT T.Text a b
-- err s = throwError $ T.pack s

functionNamesCheck :: Program -> FunctionM ()
functionNamesCheck p@(Program topDefs) = do
  let names = getFunctionNames p
  let reps = repeated names
  when (reps /= []) (throwError $ T.pack $ "function names not unique: " ++ (show reps))
  when (not $ "main" `elem` names) (throwError $ T.pack "'main' not found!")

listStmtExpressions :: Stmt -> [Expr]
listStmtExpressions = undefined

listBlockExpressions :: Block -> [Expr]
listBlockExpressions (Block stmts) = foldr (++) [] (map listStmtExpressions stmts)

listProgramExpressions :: Program -> [Expr]
listProgramExpressions (Program topDefs) = foldr (++) []
  (map listBlockExpressions $ map (\(FnDef _ _ _ b) -> b) topDefs)


belongs :: S.Set a -> a -> Bool
belongs names name = case S.lookupIndex name names of
    Nothing -> False
    Just _ -> True

usedFunctionsNames :: [Expr] -> [String]
usedFunctionsNames [] = []
usedFunctionsNames (e:es) = case e of
  EApp (Ident id)  _ -> id:(usedFunctionsNames es)
  _ -> usedFunctionsNames es


functionUsagesCheck :: Program -> FunctionM ()
functionUsagesCheck p@(Program topDefs) = do
  funNames <- get
  let exprs = listProgramExpressions p
  let usedFunNames = usedFunctionsNames funNames exprs
  let check = \names -> \x -> when (not $ belongs x names) (throwError $ T.pack $ "usage of not defined function: " ++ x)
  mapM (funNames check) usedFunNames
  return ()
  
usedFunNames :: [Expr] -> S.Set String -> [String]
usedFunNames [] _ = [] 
usedFunNames (e:es) names = case e of
  EApp id _ -> id:(usedFunNames es names)


getFilteredRepeats :: Ord b => [a] -> (a -> Bool) -> (a -> b) -> [b]
getFilteredRepeats lst filt f = repeated $ map f (filter filt lst)


runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f

run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  let outFile = dropExtension fp <.> "myout"
  case ts of
           Bad s    -> do
             writeFile outFile "ERROR\n"
           Ok  tree -> do
             writeFile outFile "OK\n"
           
main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _  -> putStrLn "auu test"  
    -- _ -> error"args error!"

