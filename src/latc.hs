{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

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
import Data.List.Unique(repeated, isUnique)
import Data.List(elemIndex)

import Debug.Trace


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


belongs :: Ord a => S.Set a -> a -> Bool
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
  let usedFunNames = usedFunctionsNames exprs
  let check = \names -> \x -> when (not $ belongs names x) (throwError $ T.pack $ "usage of not defined function: " ++ x)
  mapM (check funNames) usedFunNames
  return ()
  
usedFunNames :: [Expr] -> S.Set String -> [String]
usedFunNames [] _ = [] 
usedFunNames (e:es) names = case e of
  EApp (Ident id) _ -> id:(usedFunNames es names)


getFilteredRepeats :: Ord b => [a] -> (a -> Bool) -> (a -> b) -> [b]
getFilteredRepeats lst filt f = repeated $ map f (filter filt lst)


checkFunctionCases :: Program -> FunctionM ()
checkFunctionCases p = do
  let funNamesLst = getFunctionNames p
  put (S.fromList funNamesLst)
  functionNamesCheck p
  return ()
  

runStateM :: StateM s a -> s -> IO (Either T.Text a)
runStateM comp s = evalStateT (runExceptT comp) s


funCheckState0 :: S.Set String
funCheckState0 = S.empty


returnsS :: Stmt -> Bool
returnsS s = case s of
  Ret _ -> True
  VRet -> True
  BStmt (Block stmts) -> True `elem` (map returnsS stmts)
  CondElse _ s1 s2 -> (returnsS s1) && (returnsS s2)
  _ -> False


returnsProperlyTopDef :: TopDef -> ExceptT T.Text IO ()
returnsProperlyTopDef (FnDef _ (Ident id) _ (Block stmts)) = do
  let rets = map returnsS stmts
  case isUnique True rets of
    Nothing -> throwError $ T.pack $ "function " ++ id ++ " does not return in all cases!" 
    Just False -> throwError $ T.pack $ "function " ++ id ++ " returns multiple times!"
    Just True -> when ((last rets) /= True) (throwError $ T.pack $ "returning statement must be the last one in function!")
  return ()

returnsProperly :: Program -> ExceptT T.Text IO ()
returnsProperly (Program topDefs) = forM_ topDefs returnsProperlyTopDef

uniqueArgsTopDef :: TopDef -> ExceptT T.Text IO ()
uniqueArgsTopDef (FnDef _ (Ident id) args _) = do
  let unpack = \(Arg _ (Ident id)) -> id
  case repeated $ map unpack args of
    [] -> return ()
    x:xs -> throwError $ T.pack $ "argument name " ++ x ++ " not unique in function " ++ id

uniqueArgs :: Program -> ExceptT T.Text IO ()
uniqueArgs (Program topDefs) = forM_ topDefs uniqueArgsTopDef

varsEnv0 :: TopDef -> [String]
varsEnv0 (FnDef _ _ args _) = map (\(Arg _ (Ident id)) -> id) args

itemId :: Item -> String
itemId (NoInit (Ident id)) = id
itemId (Init (Ident id) _) = id

uniqueVarsPerBlock :: Block -> String -> EnvM [String] ()
uniqueVarsPerBlock (Block []) funName = return ()
uniqueVarsPerBlock (Block (s:stmts)) funName = do
  let comp = uniqueVarsPerBlock (Block stmts) funName
  declared <- ask
  case s of
    Decl _ nnames -> do
      let names = map itemId nnames
      let res = map (flip elem declared) names
      case elemIndex True res of
        Nothing -> local ((++) names) comp
        Just idx -> throwError $ T.pack $ "variable " ++ (show idx) ++ " declared multiple times in function " ++ funName
    BStmt b -> uniqueVarsPerBlock b funName >> comp
    _ -> comp
  
   

uniqueVarsPerTopDef :: TopDef -> EnvM [String] ()
uniqueVarsPerTopDef (FnDef _ (Ident id) args b) = do
  let env = map (\(Arg _ (Ident id)) -> id) args
  local (\_ -> env) $ uniqueVarsPerBlock b id

uniqueVars :: Program -> EnvM [String] ()
uniqueVars (Program topDefs) = forM_ topDefs uniqueVarsPerTopDef

 
varsInExp :: Expr -> [Ident]
varsInExp e = case e of
  EVar idd@(Ident id) -> [idd]
  EApp _ exprs -> concat $ map varsInExp exprs
  Neg e -> varsInExp e
  Not e -> varsInExp e
  EMul e1 _ e2 -> (varsInExp e1) ++ (varsInExp e2)
  EAdd e1 _ e2 -> (varsInExp e1) ++ (varsInExp e2)
  ERel e1 _ e2 -> (varsInExp e1) ++ (varsInExp e2)
  EAnd e1 e2 -> (varsInExp e1) ++ (varsInExp e2)
  EOr e1 e2 -> (varsInExp e1) ++ (varsInExp e2)
  _ -> []

callsInExp :: Expr -> [Expr]
callsInExp e = case e of
  EVar _ -> []
  ee@(EApp _ es) -> concat $ [[ee]] ++ (map callsInExp es)
  Neg e -> callsInExp e
  Not e -> callsInExp e
  EMul e1 _ e2 -> (callsInExp e1) ++ (callsInExp e2)
  EAdd e1 _ e2 -> (callsInExp e1) ++ (callsInExp e2)
  ERel e1 _ e2 -> (callsInExp e1) ++ (callsInExp e2)
  EAnd e1 e2 -> (callsInExp e1) ++ (callsInExp e2)
  EOr e1 e2 -> (callsInExp e1) ++ (callsInExp e2)
  _ -> []

showId :: Ident -> String
showId (Ident id) = id

itemIdent :: Item -> Ident
itemIdent (NoInit id) = id
itemIdent (Init id _) = id


onlyDeclaredVarsUsedBlock :: Block -> Ident -> EnvM [Ident] ()
onlyDeclaredVarsUsedBlock (Block []) _ = return ()
onlyDeclaredVarsUsedBlock (Block (s:stmts)) funName = do
  let comp = onlyDeclaredVarsUsedBlock (Block stmts) funName
  declared <- ask
  let _check = \stm -> \id -> when (not $ id `elem `declared) (throwError $ T.pack $ "usage of not declared variable \'" ++ (showId id) ++ "\' in function " ++ (showId funName) ++ " in statement " ++ (show stm) ++ " with env " ++ (show declared))
  let check = _check s
  case s of
    Decl _ varItems -> do
      let vars = map itemIdent varItems 
      local (\e -> e ++ vars) comp
    BStmt b -> do
      onlyDeclaredVarsUsedBlock b funName
      comp
    Ass id e -> do
      check id
      forM_ (varsInExp e) check
      comp
    Incr id -> do
      check id
      comp
    Decr id -> do
      check id
      comp
    Ret e -> do
      forM_ (varsInExp e) check
      comp
    Cond e ss -> do
      forM_ (varsInExp e) check
      onlyDeclaredVarsUsedBlock (Block [ss]) funName
      comp
    CondElse e ss1 ss2 -> do
      forM_ (varsInExp e) check
      onlyDeclaredVarsUsedBlock (Block [ss1]) funName
      onlyDeclaredVarsUsedBlock (Block [ss2]) funName
      comp
    While e ss -> do
      forM_ (varsInExp e) check
      onlyDeclaredVarsUsedBlock (Block [ss]) funName
      comp
    SExp e -> do
      forM_ (varsInExp e) check
      comp
    _ -> comp
    


onlyDeclaredVarsUsedTopDef :: TopDef -> EnvM [Ident] ()
onlyDeclaredVarsUsedTopDef (FnDef _ id args b) = do
  let env = map (\(Arg _ id) -> id) args
  local (\_ -> env) $ onlyDeclaredVarsUsedBlock b id

-- run wih empty env, i will take care of rest
onlyDeclaredVarsUsed :: Program -> EnvM [Ident] ()
onlyDeclaredVarsUsed (Program topDefs) = forM_ topDefs onlyDeclaredVarsUsedTopDef


checkArgsNum :: Ident -> Stmt -> Expr -> StateM [TopDef] ()
checkArgsNum funName s (EApp id lst) = do
  topdefs <- get
  let names = map (\(FnDef _ id _ _) -> id) topdefs
  let errMsg = "error in function " ++ (show funName) ++ " in " ++ (show s) ++ ": "
  case elemIndex id names of
    Nothing -> throwError $ T.pack $ errMsg ++ "not defined function usage (" ++ (show id) ++ ")"
    Just idx -> case topdefs !! idx of
      (FnDef _ idd args _) -> do
        -- traceM $ (show $ length args) ++ "|||" ++ (show $ length lst)
        when ((length lst) /= (length args)) (throwError $ T.pack $ errMsg ++ "funtion " ++ (show idd) ++ "applied to " ++ (show $ length lst) ++ " arguments, but it has " ++ (show $ length args) ++ "\n")

_check :: Ident -> Stmt -> Expr -> StateM [TopDef] ()
_check id s e = do
  let calls = callsInExp e
  -- traceM $ (show e) ++ ".." ++ (show calls)
  forM_ calls (checkArgsNum id s)

checkItem :: Ident -> Stmt -> Item -> StateM [TopDef] ()
checkItem id s (Init (Ident idd) e) = do
  _check id s e
checkItem _ _ _ = return ()

properArgumentNumberCallsBlock :: Ident -> Block -> StateM [TopDef] ()
properArgumentNumberCallsBlock _ (Block []) = return ()
properArgumentNumberCallsBlock id (Block (s:stmts)) = do
  let comp = properArgumentNumberCallsBlock id (Block stmts)
  let check = _check id s
  case s of
    BStmt b -> properArgumentNumberCallsBlock id b
    Decl _ items -> forM_ items (checkItem id s)
    Ass _ e -> do
      check e
    Ret e -> do
      check e
    Cond ee ss -> do
      check ee
      properArgumentNumberCallsBlock id (Block [ss])
    CondElse ee ss1 ss2 -> do
      check ee
      properArgumentNumberCallsBlock id (Block [ss1])
      properArgumentNumberCallsBlock id (Block [ss2])
    While ee ss -> do
      check ee
      properArgumentNumberCallsBlock id (Block [ss])
    SExp e -> do
      check e
    _ -> return ()
  comp

properArgumentNumberCallsTopDef :: TopDef -> StateM [TopDef] ()
properArgumentNumberCallsTopDef (FnDef _ id _ b) = properArgumentNumberCallsBlock id b


-- run wih empty state, i will take care of rest
properArgumentNumberCalls :: Program -> StateM [TopDef] ()
properArgumentNumberCalls (Program topDefs) = do
  put topDefs
  forM_ topDefs properArgumentNumberCallsTopDef



type FunType = (Type, [Type]) -- return, args
type TypecheckEnv = (Map.Map Ident FuncType, Map.Map Ident Type)


typeCheckTopDef :: TopDef -> TypeEnv ()
typeCheckTopDef (FnDef t id args b) = do
  let b0 = Map.fromList $ map (\(Arg t id) -> ((b, id), t)) args
  modifyVar $ const b0
  createTypeEnvBlock b




--------------------------------------------------------------

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
             resReturn <- runExceptT (returnsProperly tree)
             resArgs <- runExceptT (uniqueArgs tree)
             resUniqueVars <- runReaderT (runExceptT $ uniqueVars tree) []
             resDeclaredVars <- runReaderT (runExceptT $ onlyDeclaredVarsUsed tree) []
             resProperCallNum <- runStateM (properArgumentNumberCalls tree) []
             let res = [resFunctions,
                        resReturn,
                        resArgs,
                        resUniqueVars,
                        resDeclaredVars,
                        resProperCallNum]
             
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

