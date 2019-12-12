{-
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-}

module Frontend where

import AbsLatte
import Types
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


-- type EnvM e   = ExceptT T.Text (ReaderT e IO)
type EnvM e = ReaderT e (ExceptT T.Text IO)
-- type StateM s = ExceptT T.Text (StateT s IO)
type StateM s = StateT s (ExceptT T.Text IO)


runStateM :: StateM s a -> s -> ExceptT T.Text IO a
runStateM comp s = evalStateT comp s

type FunctionCheckM = StateM (S.Set String)

type FunctionM = StateM (S.Set String)



getFunctionNames :: Program -> [String]
getFunctionNames (Program topDefs) = map (\(FnDef _ (Ident name) _ _ ) -> name) topDefs

checkFunctionNamesUnique :: Program -> S.Set String -> Bool
checkFunctionNamesUnique p@(Program topDefs) s = (length topDefs) == (S.size s)


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

uniqueVarsPerBlock :: Block -> TopDef -> EnvM [String] ()
uniqueVarsPerBlock (Block []) _ = return ()
uniqueVarsPerBlock (Block (s:stmts)) f@(FnDef _ (Ident funName) _ _) = do
  let comp = uniqueVarsPerBlock (Block stmts) f
  declared <- ask
  case s of
    Decl _ nnames -> do
      let names = map itemId nnames
      let res = map (flip elem declared) names
      case elemIndex True res of
        Nothing -> local ((++) names) comp
        Just idx -> throwError $ T.pack $ "variable " ++ (show (names !! idx)) ++ " declared multiple times in function " ++ funName
    BStmt b -> local (\_ -> argsEnv0 f) (uniqueVarsPerBlock b f) >> comp
    _ -> comp

argsEnv0 :: TopDef -> [String]
argsEnv0 (FnDef _ _ args _) = map (\(Arg _ (Ident id)) -> id) args 

uniqueVarsPerTopDef :: TopDef -> EnvM [String] ()
uniqueVarsPerTopDef f@(FnDef _ _ _ b) = do
  let env = argsEnv0 f
  local (\_ -> env) $ uniqueVarsPerBlock b f

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
type TypeCheckEnv = (Location, Map.Map Ident FunType, Map.Map Ident Type)


errorTypecheck :: Location -> Maybe Expr -> Type -> Type -> T.Text
errorTypecheck loc ee actual shouldbe = case ee of
  Just e -> T.pack $ printf "%s type mismatch in expression %s (got %s, should be %s)" (show loc) (show e) (show actual) (show shouldbe)
  Nothing -> T.pack $ printf "%s type mismatch (got %s, should be %s)" (show loc) (show actual) (show shouldbe)


typeCheckBlock :: Block -> EnvM TypeCheckEnv ()
typeCheckBlock (Block []) = return ()
typeCheckBlock (Block (s:stmts)) = do
  (loc, fenv, venv) <- ask
  case s of
    Empty -> typeCheckBlock (Block stmts)
    BStmt b -> (typeCheckBlock b) >> (typeCheckBlock (Block stmts))
    Decl t items -> do
      let checkItem = \t2 -> \it -> case it of
            NoInit id -> return ()
            Init id e -> do
              tt <- inferType e
              when (t2 /= tt) $ throwError $ errorTypecheck loc (Just e) tt t2
      forM_ items (checkItem t)
      let v' = Map.fromList $ zip (map itemIdent items) (repeat t)
      local (\(l,f,v) -> (l,f, Map.union v' v)) $ typeCheckBlock (Block stmts)
    Ass id e -> do
      t <- inferType e
      case Map.lookup id venv of
        Nothing -> throwError $ T.pack $ printf "error in %s: usage of not defined variable %s" (show loc) (show id)
        Just tt -> do
          when (t /= tt) $ throwError $ errorTypecheck loc (Just e) t tt
          typeCheckBlock (Block stmts)
    Incr id -> do
      case Map.lookup id venv of
        Nothing -> throwError $ T.pack $ printf "error in %s: usage of not defined variable %s" (show loc) (show id)
        Just t -> do
          when (t /= Int) $ throwError $ errorTypecheck loc Nothing t Int
          typeCheckBlock (Block stmts)
    Decr id -> do
      case Map.lookup id venv of
        Nothing -> throwError $ T.pack $ printf "error in %s: usage of not defined variable %s" (show loc) (show id)
        Just t -> do
          when (t /= Int) $ throwError $ errorTypecheck loc Nothing t Int
          typeCheckBlock (Block stmts)
    Ret e -> do
      t <- inferType e
      case loc of
        FunName id -> do
          let (tt, _) = fenv Map.! id
          when (t /= tt) $ throwError $ T.pack $ printf "error in %s: return type mismatch (got %s, should be %s)" (show loc) (show t) (show tt)
        _ -> error "NOT IMPLEMENTED"
      typeCheckBlock (Block stmts)
    VRet -> do
      case loc of
        FunName id -> do
          let (tt, _) = fenv Map.! id
          when (Void /= tt) $ throwError $ T.pack $ printf "error in %s: return type mismatch (got %s, should be %s)" (show loc) (show Void) (show tt)
        _ -> error "NOT IMPLEMENTED"
      typeCheckBlock (Block stmts)
    Cond e ss -> do
      t <- inferType e
      when (t /= Bool) $ throwError $ T.pack $ printf "error in %s: must be boolean in if condition, got %s" (show loc) (show t)
      typeCheckBlock (Block [ss])
      typeCheckBlock (Block stmts)
    CondElse e ss1 ss2 -> do
      t <- inferType e
      when (t /= Bool) $ throwError $ T.pack $ printf "error in %s: must be boolean in if condition, got %s" (show loc) (show t)
      typeCheckBlock (Block [ss1])
      typeCheckBlock (Block [ss2])
      typeCheckBlock (Block stmts)
    While e ss -> do
      t <- inferType e
      when (t /= Bool) $ throwError $ T.pack $ printf "error in %s: must be boolean in while condition, got %s" (show loc) (show t)
      typeCheckBlock (Block [ss])
      typeCheckBlock (Block stmts)
    SExp e -> do
      _ <- inferType e
      typeCheckBlock (Block stmts)


typeCheckTopDef :: TopDef -> EnvM TypeCheckEnv ()
typeCheckTopDef (FnDef t id args b) = do
  local (\(_, f, _) -> (FunName id, f, v)) (typeCheckBlock b) where
    v = Map.fromList $ map (\(Arg t id) -> (id, t)) args


getFuncType :: TopDef -> FunType
getFuncType (FnDef t _ args _) = (t, argTs) where
  argTs = map (\(Arg tt _) -> tt) args


checkMain :: EnvM TypeCheckEnv ()
checkMain = do
  (loc, fenv, _) <- ask
  let comp = \(Ident name, (retType, ts)) -> case name of
                                               "main" -> when (retType /= Int ) $ throwError $ T.pack $ "'main' type must be int!"
                                               _      -> return ()
  forM_ (Map.toList fenv) comp 
                          
  
typeCheck :: Program -> EnvM TypeCheckEnv ()
typeCheck (Program topDefs) = local (\(l,_,v) -> (l,fenv,v)) $ checkMain >> forM_ topDefs typeCheckTopDef where
  fenv = Map.fromList $ map (\a@(FnDef _ id _ _) -> (id, getFuncType a)) topDefs

inferType :: Expr -> EnvM TypeCheckEnv Type
inferType e = do
  (loc, fenv, venv) <- ask
  let errPrefix = show loc
  case e of
    EVar id -> do
      case Map.lookup id venv of
        Nothing -> throwError $ T.pack $ printf "error in %s: usage of not defined variable %s" (show loc) (show id)
        Just res -> return res
    ELitInt _ -> return (Int :: Type)
    ELitTrue -> return (Bool :: Type)
    ELitFalse -> return (Bool :: Type)
    EApp id exprs -> do
      types <- forM exprs inferType
      let fun = fenv Map.! id
      if types /= (snd fun)
        then throwError $ T.pack $ printf "%s function %s application arguments type mismatch: expected %s and obtained %s" errPrefix (show id) (show (snd fun)) (show types)
        else return $ fst fun
    EString _ -> return (Str :: Type)
    Neg e -> do
      t <- inferType e
      if t == Int then return Int else throwError $ T.pack $ printf "cannot negate %s type value in expression %s (must be Integer)" (show t) (show e)
    Not e -> do
      t <- inferType e
      if t == Bool then return Bool else throwError $ T.pack $ printf "cannot negate %s type value in expression %s (must be Boolean)" (show t) (show e)
    EMul e1 _ e2 -> do
      t1 <- inferType e1
      t2 <- inferType e2
      if t1 == Int && t2 == Int then return Int else throwError $ T.pack $ printf $ "TODO"
    EAdd e1 op e2 -> do
      t1 <- inferType e1
      t2 <- inferType e2
      case (t1,t2,op) of
            (Str,Str,Minus) -> throwError $ T.pack $ printf "error in %s: cannot subtract strings in %s!" (show loc) (show e)
            (Str,Str,Plus)  -> return Str
            (Int,Int,_)     -> return Int
            _ -> throwError $ T.pack $ printf "error in %s: add type mismatch (%s + %s) in %s!" (show loc) (show t1) (show t2) (show e)
    ERel e1 relOp e2 -> do
      t1 <- inferType e1
      t2 <- inferType e2
      case (t1,t2,relOp) of
        (Int,Int,_) -> return Int
        (Str,Str,_) -> return Str
        _ -> throwError $ T.pack $ printf "error in %s: type mismatch during comparision (%s and %s) in %s" (show loc) (show t1) (show t2) (show e)
    EAnd e1 e2 -> do
      t1 <- inferType e1
      t2 <- inferType e2
      if t1 == Bool && t2 == Bool then return Bool else throwError $ T.pack $ printf "error in %s: logical AND type mismatch (%s + %s) in %s!" (show loc) (show t1) (show t2) (show e)
    EOr e1 e2 -> do
      t1 <- inferType e1
      t2 <- inferType e2
      if t1 == Bool && t2 == Bool then return Bool else throwError $ T.pack $ printf "error in %s: logical OR type mismatch (%s + %s) in %s!" (show loc) (show t1) (show t2) (show e)



checkAll :: Program -> ExceptT T.Text IO ()
checkAll tree = do
  resFunctions <- runStateM (checkFunctionCases tree) funCheckState0
  resReturn <- returnsProperly tree
  resArgs <- uniqueArgs tree
  resUniqueVars <- runReaderT (uniqueVars tree) []
  resDeclaredVars <- runReaderT (onlyDeclaredVarsUsed tree) []
  resProperCallNum <- runStateM (properArgumentNumberCalls tree) []
  typeCheck <- runReaderT
    (typeCheck tree)
    (FunName (Ident "dummy"), Map.empty, Map.empty)
  return ()
  
