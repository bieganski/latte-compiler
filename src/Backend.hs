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
import qualified Data.List.Unique as LU

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
             Integer)
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
      VReg _ -> jumpAndOr e2 v1 NE        
  Abs.EOr e1 e2 -> do
    (_, v1) <- genExp e1
    case v1 of
      VBool True -> return (TBool, VBool True)
      VBool False -> genExp e2
      VReg _ -> jumpAndOr e2 v1 EQU
      

jumpAndOr :: Abs.Expr -> LLVMVal -> RelOp -> GenM LLVMTypeVal
jumpAndOr e2 v1 op = do
        f <- getFresh
        emit $ Cmp (VReg f) op TBool v1 (VInt 0)
        (ins,n,m,blockNum) <- get
        branch1 <- getFresh
        -- branch2 <- getFresh // we cannot do that, because register numbers must be +1 each
        let cond = \b2 -> BrCond (TBool, VReg f) (TLabel, VLabel branch1) (TLabel, VLabel b2)
        put ([],n+1,m,branch1) -- entering new block
        emit $ Comment $ "im in block " ++ (show branch1)
        (_, v2) <- genExp e2 -- during that, new instructions were added
        (e2ins,_,_,r) <- get -- these are new ones (cause we started with [] list)
        branch2 <- getFresh
        let allIns = concat [[cond branch2],
                             reverse e2ins,
                             [Br (TLabel, VLabel branch2)]] -- in proper order
        (_,n,m,b) <- get
        put (reverse $ (reverse ins) ++ allIns,n,m,branch2)
        emit $ Comment $ "im in block " ++ (show branch2)
        res <- getFresh
        emit $ Phi (VReg res) TBool [(v1, VLabel blockNum), (v2, VLabel r)]
        return (TBool, VReg res)

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



data AssignedCheck = AssCheck { inner :: [Abs.Ident], result :: [Abs.Ident] } 

-- list context variables (already existing), that are
-- assigned in given block (useful during PHI computation)
-- second argument is internal accumulator of block-declared ones,
-- thus call with empty list.
varsAssigned :: Abs.Stmt -> State AssignedCheck ()
varsAssigned s = do
  AssCheck i r <- get
  case s of
    Abs.Decl _ items -> modify \st -> st {inner = inner st ++ map (^.Abs.iid) items}
    Abs.Ass id _ -> case id `elem` i of
      False -> modify \st -> st {result = id : (result st)}
      True -> return ()
    Abs.BStmt (Abs.Block stmts) -> forM_ stmts varsAssigned
    Abs.Incr id -> case id `elem` i of
      False -> modify \st -> st {result = id : (result st)}
      True -> return () 
    Abs.Decr id -> case id `elem` i of
      False -> modify \st -> st {result = id : (result st)}
      True -> return ()
    _ -> return ()


genStmtEnhanced :: Abs.Block -> ([Abs.Ident], Map.Map Abs.Ident LLVMTypeVal) -> GenM ([Abs.Ident], Map.Map Abs.Ident LLVMTypeVal)
genStmtEnhanced (Abs.Block []) st = return st
genStmtEnhanced (Abs.Block (s:stmts)) (inner, res) = do
  env@(fenv, venv) <- ask
  case s of
    Abs.Decl t items -> do
      env' <- foldM declChangeEnv env $ zip (repeat t) items
      local (const env') $ genStmtEnhanced (Abs.Block stmts) (inner ++ map (^.Abs.iid) items, res)
    Abs.Ass id e -> case id `elem` inner of
      True -> genStmtEnhanced (Abs.Block stmts) (inner, res)
      False -> do
        (t,v) <- genExp e
        let venv' = Map.insert id (t,v) venv
        local (const (fenv, venv')) $ genStmtEnhanced (Abs.Block stmts) (inner, Map.insert id (t,v) res)
    Abs.BStmt (Abs.Block ss) -> do
      (inner2, m2) <- genStmtEnhanced (Abs.Block ss) (inner, res)
      genStmtEnhanced (Abs.Block stmts) (inner2, m2)
    Abs.Decr id -> do
      (t, val) <- doAdd id (-1)
      let venv' = Map.insert id (t, val) venv
      let comp = case id `elem` inner of
            True -> genStmtEnhanced (Abs.Block stmts) (inner, res)
            False -> do
              genStmtEnhanced (Abs.Block stmts) (inner, Map.insert id (t,val) res)
      local (const (fenv, venv')) comp
    Abs.Incr id -> do
      (t, val) <- doAdd id 1
      let venv' = Map.insert id (t, val) venv
      let comp = case id `elem` inner of
            True -> genStmtEnhanced (Abs.Block stmts) (inner, res)
            False -> do
              genStmtEnhanced (Abs.Block stmts) (inner, Map.insert id (t,val) res)
      local (const (fenv, venv')) comp
    _ -> do
      genStmt (Abs.Block (s:stmts))
      return (inner, res)
      


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
      _ <- getFresh
      emit $ Ret (TVoid, VDummy)
      comp
    Abs.SExp e -> do
      genExp e
      comp
    Abs.CondElse e s1 s2 -> do
      (TBool,v) <- genExp e
      f <- getFresh
      emit $ Cmp (VReg f) NE TBool v (VInt 0)
      trueLabel <- getFresh
      let jump = \false -> BrCond (TBool, VReg f) (TLabel, VLabel trueLabel) (TLabel, VLabel false)
      (ins,n0,m0,b) <- get
      let AssCheck _ assigned1 = execState (varsAssigned s1) $ AssCheck [] []
      let AssCheck _ assigned2 = execState (varsAssigned s2) $ AssCheck [] []
      let phiNodes = LU.unique $ L.intersect assigned1 assigned2
      put ([],n0,m0,trueLabel) -- entering new block
      emit $ Comment $ "block " ++ (show trueLabel)
      (_, mm1) <- genStmtEnhanced (Abs.Block [s1]) ([], Map.empty)
      falseLabel <- getFresh
      (s1ins,n1,m1,s1b) <- get
      put ([],n1,m1,falseLabel)
      emit $ Comment $ "block " ++ (show falseLabel)
      (_, mm2) <- genStmtEnhanced (Abs.Block [s2]) ([], Map.empty)
      finishLabel <- getFresh
      (s2ins,n2,m2,s2b) <- get
      let allIns = concat [[jump falseLabel],
                           reverse s1ins,
                           [Br (TLabel, VLabel finishLabel)],
                           reverse s2ins,
                           [Br (TLabel, VLabel finishLabel)]]
      
      tvs <- mapM getVar phiNodes
      _phiIns <- mapM (\((t,v), id) -> constructPhi mm1 mm2 (VLabel s1b) (VLabel s2b) (id,t,v)) $ zip tvs phiNodes
      let phiIns = map snd _phiIns
      let newMap = Map.fromList $ map fst _phiIns
      put ((reverse phiIns) ++ (reverse allIns) ++ ins,n2,m2,finishLabel)
      emit $ Comment $ "block " ++ (show finishLabel)
      local (\(fenv, venv) -> (fenv, Map.union newMap venv)) comp
    Abs.Cond e s1 -> do
      genStmt $ (Abs.Block [Abs.CondElse e s1 Abs.Empty])
      comp
    Abs.While e s -> do
      let AssCheck _ _phiNodes = execState (varsAssigned s) $ AssCheck [] []
      let phiNodes = LU.unique $ _phiNodes
      (_,_,_,startb) <- get
      be <- getFresh
      emit $ Br (TLabel, VLabel be)
      emit $ Comment $ "block" ++ (show be)
      (ins,n0,m0,b) <- get
      let ts = map fst $ map (\id -> venv Map.! id) phiNodes   
      let mapUpdate = Map.fromList $ zip phiNodes $ zip ts $ map VReg [n0..]
      put ([],n0 + toInteger (length phiNodes),m0,be)
      (te, ve) <- local (\(f,v) -> (f, Map.union mapUpdate v)) $ genExp e
      vecmp <- getFresh
      emit $ Cmp (VReg vecmp) NE TBool ve (VInt 0)
      sLabel <- getFresh
      -- endLabel = ??
      (inse,ne,me,be_end) <- get
      put ([],ne,me,sLabel)
      emit $ Comment $ "block" ++ (show sLabel)
      (_, svenv) <- local (\(f,v) -> (f, Map.union mapUpdate v)) $ genStmtEnhanced (Abs.Block [s]) ([], Map.empty)
      (_,_,_,bs_end) <- get
      endLabel <- getFresh
      emit $ Br (TLabel, VLabel be)
      emit $ Comment $ "block " ++ (show endLabel)
      tvs <- mapM getVar phiNodes
      (itmp,ntmp,mtmp,btmp) <- get
      put (itmp,n0,mtmp,btmp)
      _phis <- mapM (\((t,v), id) -> constructPhi svenv venv (VLabel bs_end) (VLabel startb) (id,t,v)) $ zip tvs phiNodes
      put (itmp,ntmp,mtmp,btmp)
      let phis = map snd _phis
      let newMap = Map.fromList $ map fst _phis
      (inss,ns,ms,bs_end) <- get
      let ejump = BrCond (TBool, VReg vecmp) (TLabel, VLabel sLabel) (TLabel, VLabel endLabel)
      let allIns = concat [inss ++ (ejump:inse) ++ (reverse phis) ++ ins]
      put (allIns,ns,ms,endLabel)
      local (\(fenv, venv) -> (fenv, Map.union newMap venv)) comp


prefix :: Eq a => [a] -> [a] -> Bool 
prefix [] _ = True
prefix (y:ys) (x:xs)  
  | x == y    = prefix ys xs 
  | otherwise = prefix (y:ys) xs

def :: LLVMType -> LLVMVal
def t = case t of
  TVoid -> VDummy
  TBool -> VBool False
  TInt -> VInt 0

fixEmptyBlock :: LLVMType -> [Instr] -> [Instr]
fixEmptyBlock rett (FunEnd:((Comment s):rest)) = (FunEnd:(a:((Comment s):rest)))
  where a = Ret (rett, def rett)
fixEmptyBlock _ a = a

constructPhi :: Map.Map Abs.Ident LLVMTypeVal -> Map.Map Abs.Ident LLVMTypeVal ->
  LLVMVal -> LLVMVal ->
  (Abs.Ident, LLVMType, LLVMVal) -> GenM ((Abs.Ident, LLVMTypeVal), Instr) 
constructPhi m1 m2 b1 b2 (id,t,v) = do
  f <- getFresh
  let (_, v1) = m1 Map.! id
  let (_, v2) = m2 Map.! id
  return ((id, (t, VReg f)), Phi (VReg f) t [(v1, b1), (v2, b2)])
  

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
      return (t, (VReg f))


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
  put (ins, toInteger (1 + (length args)), m, toInteger $ length args)
  flip local comp $ \(fenv, _) -> (fenv, createFunctionEnv args)
  (ins2,n,m,b) <- get
  put (fixEmptyBlock (mapType ret) ins2,n,m,b)
  

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
