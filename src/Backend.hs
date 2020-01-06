{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Backend where

import qualified AbsLatte as Abs

import Types
import Utils
import System.FilePath
import Data.List as L

import Frontend(itemIdent)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fail
import Control.Lens

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Debug.Trace



type FuncEnv = Map.Map Abs.Ident LLVMType

absTypeToLLVM :: Abs.Type -> LLVMType
absTypeToLLVM t = case t of
            Abs.Int -> TInt
            Abs.Str -> TPtr TChar
            Abs.Bool -> TBool
            Abs.Void -> TVoid
            _ -> error "not implemented (absTypeToLLVM)"

getFresh :: GenM Integer
getFresh = do
  (_,num,_,_) <- get
  modify \(c, n, m, b) -> (c, n+1, m, b)
  return num



type GenE = (FuncEnv,
             Map.Map Abs.Ident LLVMTypeVal)
-- function env and variable env

type GenS = ([Instr],
             Integer,
             Map.Map LLVMVal String,
             Int)
-- (code generated,
-- num of fresh variable,
-- Map for global string literals)
-- number of current block

type GenM = ReaderT GenE (StateT GenS (Except T.Text))


instance MonadFail Identity where
    fail :: String -> m a
    fail = error

getVar :: Abs.Ident -> GenM LLVMTypeVal
getVar id = do
  (_, m) <- ask
  return $ m Map.! id

getFunType :: Abs.Ident -> GenM LLVMType
getFunType id = do
  (m, _) <- ask
  return $ m Map.! id


debug = (TInt, VInt 2137)


addGlobStr :: String -> GenM LLVMVal
addGlobStr s = do
  (ins,n,m,b) <- get
  let glob = VGlobStr $ toInteger $ Map.size m
  case take 1 $ Map.toList $ Map.filter (==s) m of
    [] -> do
      put (ins,n,Map.insert glob s m,b)
      return $ VGlobStr $ toInteger $ Map.size m
    [(VGlobStr num, _)] -> return $ VGlobStr num 


getExprVars :: Abs.Expr -> Set.Set Abs.Ident
getExprVars e = case e of
  Abs.EVar id -> Set.singleton id
  Abs.EApp _ es -> Set.unions $ map getExprVars es
  Abs.Neg e -> getExprVars e
  Abs.Not e -> getExprVars e
  Abs.EMul e1 _ e2 -> Set.union (getExprVars e1) (getExprVars e2)
  Abs.EAdd e1 _ e2 -> Set.union (getExprVars e1) (getExprVars e2)
  Abs.ERel e1 _ e2 -> Set.union (getExprVars e1) (getExprVars e2)
  Abs.EAnd e1 e2 -> Set.union (getExprVars e1) (getExprVars e2)
  Abs.EOr e1 e2 -> Set.union (getExprVars e1) (getExprVars e2)
  _ -> Set.empty

getCommonVars :: Abs.Expr -> Abs.Expr -> Set.Set Abs.Ident
getCommonVars e1 e2 = Set.intersection (getExprVars e1) (getExprVars e2)


genExp :: Abs.Expr -> GenM LLVMTypeVal
genExp e = case e of
  Abs.EVar id -> getVar id
  Abs.ELitInt n -> return (TInt, VInt n)
  Abs.ELitTrue -> return (TBool, VBool True)
  Abs.ELitFalse -> return (TBool, VBool False)
  Abs.EApp id@(Abs.Ident iid) exps -> do
    tvs <- forM exps genExp
    TFun ret args <- getFunType id
    case ret of
      TVoid -> do
        emit $ FunCall VVoid TVoid iid tvs
        return (TVoid, VVoid)
      _ -> do
        fresh <- getFresh
        emit $ FunCall (VReg fresh) ret iid tvs
        return (ret, VReg fresh)
  Abs.EString s -> do
    v@(VGlobStr num) <- addGlobStr s
    f <- getFresh
    let tarr = (TArr (toInteger (1 + length s)) TChar)
    emit $ GetElemPtr (VReg f) tarr [(TPtr tarr, v), (TInt, VInt 0), (TInt, VInt 0)]
    return (tstr, VReg f)
  Abs.Neg e -> genExp $ Abs.EAdd (Abs.ELitInt 0) Abs.Minus e
  Abs.Not e -> do
    (t,v) <- genExp e
    case v of
      VBool True -> return (TBool, VBool False)
      VBool False -> return (TBool, VBool True)
      VReg n -> do
        fresh <- getFresh
        emit $ Cmp (VReg fresh) NE TBool (VReg n) (VInt 0)
        return (TBool, VReg fresh)
  Abs.EMul e1 _op e2 -> do
    (t1,v1) <- genExp e1
    (t2,v2) <- genExp e2
    case (v1,v2) of
      (VInt n, VInt m) -> return (TInt, VInt $ n * m)
      _ -> do
        fresh <- getFresh
        let op = case _op of
              Abs.Times -> Times
              Abs.Div   -> Div
              Abs.Mod   -> Mod
        emit $ Bin (VReg fresh) op TInt v1 v2
        return (TInt, VReg fresh)
  Abs.ERel e1 _op e2 -> do
    (t1,v1) <- genExp e1
    (t2,v2) <- genExp e2
    let op = case _op of
          Abs.LTH -> LTH
          Abs.LE  -> LE
          Abs.GTH -> GTH
          Abs.GE  -> GE
          Abs.EQU -> EQU
          Abs.NE  -> NE
    case (v1,v2) of
      (VInt _, VInt _) -> return $ computeRelOp v1 v2 op
      _ -> do
        f <- getFresh
        emit $ Cmp (VReg f) op TInt v1 v2
        return (TBool, (VReg f))
  Abs.EAdd e1 _op e2 -> do
    let op = case _op of
          Abs.Plus -> Plus
          Abs.Minus -> Minus
    (t1,v1) <- genExp e1
    (t2,v2) <- genExp e2
    case (t1, t2) of
      (TInt, TInt) -> do
        f <- getFresh
        emit $ Bin (VReg f) op TInt v1 v2
        return (TInt, VReg f)
      (TPtr TChar, TPtr TChar) -> concatStrings (t1,v1) (t2,v2)
  Abs.EAnd e1 e2 -> do
    (_, v1) <- genExp e1
    case v1 of
      VBool True -> genExp e2
      VBool False -> return (TBool, VBool False)
      VReg _ -> do -- lazy
        f <- getFresh
        emit $ Cmp (VReg f) NE TBool v1 (VInt 0)
        let comm = getCommonVars e1 e2 -- will be generating phi for them
        (ins,n,m,blockNum) <- get
        put ([],n,m,blockNum)
        branchTrue <- getFresh
        -- branchFalse <- getFresh // we cannot do that, because register numbers must be +1 each
        let i = \falseReg -> BrCond (TBool, VReg f) (TLabel, VLabel branchTrue) (TLabel, VLabel falseReg)
        (_, v2) <- genExp e2 -- during that, new instructions were added
        branchFalse <- getFresh
        (e2ins,_,_,_) <- get
        let allIns = concat [i branchFalse,
                             reverse e2ins,
                             Br (TLabel, VLabel branchFalse)] -- in proper order
        -- TODO tutaj insert phi nodes
        (_,n,m,b) <- get
        put (diff ++ [i branchFalse] ++_ins1, n,m,b) 
        
        return debug
        
    
  _ -> error "not implemented exp"


tstr = TPtr TChar

-- http://www.cplusplus.com/reference/cstring/strcpy/ and /strlen and /strcat 
concatStrings :: LLVMTypeVal -> LLVMTypeVal -> GenM LLVMTypeVal
concatStrings (t1, v1) (t2, v2) = do
  len1 <- getFresh
  emit $ FunCall (VReg len1) TInt "_strlen" [(t1, v1)]
  len2 <- getFresh
  emit $ FunCall (VReg len2) TInt "_strlen" [(t2, v2)]
  _len <- getFresh
  emit $ Bin (VReg _len) Plus TInt (VReg len1) (VReg len2)
  len <- getFresh
  emit $ Bin (VReg  len) Plus TInt (VReg _len) (VInt 1)
  newStr <- getFresh
  emit $ FunCall (VReg newStr) tstr "_malloc" [(TInt, VReg len)]
  newStrDup <- getFresh
  emit $ FunCall (VReg newStrDup) tstr "_strcpy" [(tstr, (VReg newStr)), (t1, v1)]
  res <- getFresh
  emit $ FunCall (VReg res) tstr "_strcat" [(tstr, VReg newStrDup), (t2, v2)]
  return (tstr, VReg newStrDup)

computeRelOp :: LLVMVal -> LLVMVal -> RelOp -> LLVMTypeVal
computeRelOp (VInt v1) (VInt v2) op = let v = case op of
                                           LTH -> v1 < v2
                                           LE -> v1 <= v2
                                           GTH -> v1 > v2
                                           GE -> v1 >= v2
                                           EQU -> v1 == v2
                                           NE -> v1 /= v2
                                     in (TBool, VBool v)


genStrDecl = \((VGlobStr n), s) -> flip GlobStrDecl s $ VGlobStr $ toInteger $ n

emitGlobalStrDecls :: GenM ()
emitGlobalStrDecls = do
  (ins,n,m,b) <- get
  let ins2 = ins ++ (reverse $ map genStrDecl $ Map.toList m)
  put (ins2,n,m,b)

emit :: Instr -> GenM ()
emit i = do
  (ins,n,m,b) <- get
  put (i:ins,n,m,b)

-- STATEMENTS GENERATION -------------

mapType :: Abs.Type -> LLVMType
mapType t = case t of
        Abs.Int -> TInt
        Abs.Str -> TPtr TChar
        Abs.Bool -> TBool
        Abs.Void -> TVoid
        _ -> error "internal error mapType"

defaultVal :: LLVMType -> LLVMVal
defaultVal t = case t of
  TInt -> VInt 0
  TPtr TChar -> VGlobStr 0
  TBool -> VBool False
  TVoid -> VVoid

declChangeEnv :: GenE -> (Abs.Type, Abs.Item) -> GenM GenE
declChangeEnv (fenv,venv) (t, (Abs.NoInit id)) = do
  let val = defaultVal $ mapType t 
  return (fenv, Map.insert id (mapType t, val) venv)
declChangeEnv (fenv,venv) (_t, (Abs.Init id e)) = do
  let t = mapType _t 
  (_, v) <- genExp e
  return (fenv, Map.insert id (t, v) venv)


genStmt :: Abs.Block -> GenM ()
genStmt (Abs.Block []) = return ()
genStmt (Abs.Block (s:ss)) = do
  let comp = genStmt (Abs.Block ss)
  env@(fenv, venv) <- ask
  case s of
    Abs.Empty -> do
      comp
    Abs.BStmt b -> do
      genStmt b
      comp
    Abs.Decl t items -> do
      env' <- foldM declChangeEnv env $ zip (repeat t) items
      local (const env') comp
    Abs.Ass id e -> do
      (t,v) <- genExp e
      let venv' = Map.insert id (t,v) venv
      local (const (fenv, venv')) comp
    Abs.Incr id -> do
      (t, val) <- doAdd id 1
      let venv' = Map.insert id (t, val) venv
      local (const (fenv, venv')) comp
    Abs.Decr id -> do
      (t, val) <- doAdd id (-1)
      let venv' = Map.insert id (t, val) venv
      local (const (fenv, venv')) comp
    Abs.Ret e -> do
      (t, v) <- genExp e
      emit $ Ret (t, v)
      comp
    Abs.VRet -> do
      emit $ Ret (TVoid, VDummy)
      comp
    Abs.SExp e -> do
      genExp e
      comp
    c -> error $ "not implemented stmt" ++ (show c)


doAdd :: Abs.Ident -> Integer -> GenM LLVMTypeVal
doAdd id x = do
  (fenv,venv) <- ask
  let (t, v) = venv Map.! id
  case v of
    VInt n -> do
      return (t, VInt $ n + x)
    VReg n -> do
      f <- getFresh
      emit $ Bin (VReg f) Plus TInt (VReg n) (VInt x)
      return (t, (VReg n))


t0 :: Abs.Program -> FuncEnv
t0 (Abs.Program topDefs) = Map.union builtInFunctions $ Map.fromList $ map f topDefs  where
  f = \(Abs.FnDef ret id args _) -> (id, TFun (absTypeToLLVM ret) (map absTypeToLLVM (map (^.Abs.t) args)))
                                       

e0 :: Abs.Program -> GenE
e0 p = (t0 p, Map.empty)
      
s0 :: GenS
s0 = ([], 1, Map.singleton (VGlobStr 0) "", 1)
  
builtInFunctions = Map.fromList
  [(Abs.Ident "printString", TFun TVoid [tstr]),
   (Abs.Ident "printInt",    TFun TVoid [TInt]),
   (Abs.Ident "readInt",     TFun TInt []),
   (Abs.Ident "readString",  TFun tstr []),
   (Abs.Ident "error",       TFun TVoid []),
   (Abs.Ident "_strlen",     TFun TInt [tstr]),
   (Abs.Ident "_malloc",     TFun tstr [TInt]),
   (Abs.Ident "_strcat",     TFun tstr [tstr, tstr]),
   (Abs.Ident "_strcmp",     TFun TInt [tstr, tstr]),
   (Abs.Ident "_strcpy",     TFun tstr [tstr, tstr])]

 
buildIR :: FilePath -> T.Text -> T.Text
buildIR filename content = buildText [buildLines $ prolog (show filename),
                                      content,
                                      buildLines epilog]


runGenM :: Abs.Program -> GenM a -> Except T.Text a
runGenM p comp = evalStateT (runReaderT comp (e0 p)) s0

createFunctionEnv :: [Abs.Arg] -> Map.Map Abs.Ident LLVMTypeVal
createFunctionEnv args = Map.fromList $ map f (zip [0..] args)
  where f = \(num, (Abs.Arg t id)) -> (id, (mapType t,VReg num))

emitTopDefIR :: Abs.TopDef -> GenM ()
emitTopDefIR (Abs.FnDef ret (Abs.Ident id) args block) = do
  emit $ FunEntry id $ TFun (mapType ret) (map (mapType . (^.Abs.t)) args)
  let comp = genStmt block >> emit FunEnd
  (ins,_,m,_) <- get
  put (ins, toInteger (1 + (length args)), m, 1)
  flip local comp $ \(fenv, _) -> (fenv, createFunctionEnv args)
  

emitProgramIR :: FilePath -> Abs.Program -> GenM T.Text
emitProgramIR fp (Abs.Program topDefs) = do
  forM_ topDefs emitTopDefIR
  emitGlobalStrDecls
  (ins,_,_,_) <- get
  let decls = map (\(Abs.Ident a, b) -> Declare a b) $ Map.toList builtInFunctions
  let content = buildLines $ map show $ decls ++ (reverse ins)
  return $ buildIR fp content
  
runBackend :: FilePath -> Abs.Program -> Either T.Text T.Text
runBackend fp p = runExcept $ runGenM p (emitProgramIR fp p)
