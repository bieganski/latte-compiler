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
import Text.Printf(printf)
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

import Control.Lens.Extras(is)
import Data.Bifunctor

import Data.List (sortBy)
import Data.Function (on)

import Debug.Trace


type FuncEnv = Map.Map Abs.Ident LLVMType

getFresh :: GenM Integer
getFresh = do
  f <- gets fresh
  modify $ \s -> s {fresh = f + 1}
  return f

type VarEnv = Map.Map Abs.Ident LLVMTypeVal

-- (code generated,
-- num of fresh variable,
-- Map for global string literals)
-- number of current block
-- function env
-- variable env (per block)
-- classes env (integer is field position, counting from 0)
data GenS = GenS {ins :: [Instr],
                 fresh :: Integer,
                 lits :: Map.Map LLVMVal String,
                 currBlock :: Integer,
                 fenv :: FuncEnv,
                 venv :: VarEnv,
                 clsenv :: Map.Map Abs.Ident (Map.Map Abs.Ident (Integer, LLVMType))
                 }


type GenM = StateT GenS (Except T.Text)

instance MonadFail Identity where
    fail :: String -> m a
    fail = error

getVar :: Abs.Ident -> GenM LLVMTypeVal
getVar id = do
  m <- gets venv
  return $ m Map.! id

getFunType :: Abs.Ident -> GenM LLVMType
getFunType id = do
  m <- gets fenv
  return $ m Map.! id


addGlobStr :: String -> GenM LLVMVal
addGlobStr s = do
  m <- gets lits
  let glob = VGlobStr $ toInteger $ Map.size m
  case take 1 $ Map.toList $ Map.filter (==s) m of
    [] -> do
      modify $ \st -> st {lits = Map.insert glob s m}
      return $ VGlobStr $ toInteger $ Map.size m
    [(VGlobStr num, _)] -> return $ VGlobStr num
    
getStructWholeType :: Abs.Ident -> GenM LLVMType
getStructWholeType id = do
  let mySort = reverse . sortBy (flip compare `on` fst)
  let strT = \clsMap -> TStruct $ map snd $ mySort $ Map.elems clsMap
  m <- gets clsenv
  return $ strT $ m Map.! id

computeStructSize :: Abs.Ident -> GenM LLVMTypeVal
computeStructSize id = do
  t <- getStructWholeType id
  _s <- getFresh
  emit $ GetOffsetPtr (VReg _s) (TStructName id)
  s <- getFresh
  emit $ PtrToInt (VReg s) (TStructName id, VReg _s)
  return (TInt, VReg s)

genExp :: Abs.Expr -> GenM LLVMTypeVal
genExp e = case e of
  Abs.ENew (Abs.ClassType id) -> do
    csl <- gets clsenv
    size@(TInt, VReg x) <- computeStructSize id
    _ptr <- getFresh
    emit $ FunCall (VReg _ptr) tstr "_malloc" [size]
    ptr <- getFresh
    emit $ BitCast (VReg ptr) (TPtr TChar, VReg _ptr) (TPtr $ TStructName id)
    return (TPtr $ TStructName id, VReg ptr)
  Abs.ENull id -> do
    return (TPtr (TStructName id), VNull)
  Abs.EField (Abs.EVar clsVar) fieldId -> do
    vnv <- gets venv
    case Map.lookup clsVar vnv of
      Nothing -> undefined
      Just base@(TPtr (TStructName cName), VReg n) -> do
        cenv <- gets clsenv
        let cMap = cenv Map.! cName
        case Map.lookup fieldId cMap of
          Nothing -> throwError $ T.pack $ printf "class %s got no field %s!\n in %s" (show cName) (show fieldId) (show e)
          Just (num, typ) -> do
            fieldLoc <- getFresh
            emit $ GetElemPtr (VReg fieldLoc) (TStructName cName) [
              base,
              (TInt, VInt 0),
              (TInt, VInt num)]
            res <- getFresh
            emit $ Load (VReg res) typ (TPtr typ, VReg fieldLoc)
            return (typ, VReg res)
      Just (t,_) -> throwError $ T.pack $ printf "%s is not class-type!\n in %s" (show t) (show e)
  Abs.EField (Abs.EField e1 e2) fieldId -> throwError $ T.pack "multiple levels struct props referencing not required"
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
        emit $ Cmp (VReg fresh) EQU TBool (VReg n) (VBool False)
        return (TBool, VReg fresh)
  Abs.EMul e1 _op e2 -> do
    (t1,v1) <- genExp e1
    (t2,v2) <- genExp e2
    let op = case _op of
              Abs.Times -> Times
              Abs.Div   -> Div
              Abs.Mod   -> Mod
    case (v1,v2) of
      (VInt n, VInt m) -> case op of
        Times -> return (TInt, VInt $ n * m)
        Div -> return (TInt, VInt $ n `div` m)
        Mod -> return (TInt, VInt $ n `mod` m)
      _ -> do
        fresh <- getFresh
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
    case (t1,t2) of
      (tt@(TPtr (TStructName id)), TPtr (TStructName id2)) -> do
        case id == id2 of
          False -> throwError $ T.pack $ "frontend hole: type error during struct comparision"
          True -> do
            f <- getFresh
            emit $ Cmp (VReg f) op tt v1 v2
            return $ (TBool, VReg f)
      (TInt, TInt) -> case (v1,v2) of
        (VInt _, VInt _) -> return $ computeRelOp v1 v2 op
        _ -> do
          f <- getFresh
          emit $ Cmp (VReg f) op TInt v1 v2
          return (TBool, (VReg f))
      (TBool, TBool) -> case (v1,v2) of
        (VBool a, VBool b) -> case op of
          EQU -> return (TBool, VBool $ a == b)
          NE -> return (TBool, VBool $ a /= b)
          _ -> error "backend found frontend error: bool relop"
        _ -> do
          f <- getFresh
          emit $ Cmp (VReg f) op TBool v1 v2
          return (TBool, (VReg f))
      (_,_) -> case (v1, v2) of
        (VGlobStr s1num, VGlobStr s2num) -> do
          m <- gets lits
          let s1 = m Map.! (VReg s1num)
          let s2 = m Map.! (VReg s2num)
          case op of
            NE -> return (TBool, VBool $ s1 /= s2)
            EQU -> return (TBool, VBool $ s1 == s2)
            _ -> error "backend found frontend error: string relop"
        _ -> do
          f <- getFresh
          emit $ Cmp (VReg f) op tstr v1 v2
          return (TBool, (VReg f))
  Abs.EAdd e1 _op e2 -> do
    let op = case _op of
          Abs.Plus -> Plus
          Abs.Minus -> Minus
    (t1,v1) <- genExp e1
    (t2,v2) <- genExp e2
    case (t1, t2) of
      (TInt, TInt) -> do
        case (v1,v2) of
          (VInt n, VInt m) -> case op of
            Plus -> return (TInt, VInt $ n + m)
            Minus -> return (TInt, VInt $ n - m)
          _ -> do
            f <- getFresh
            emit $ Bin (VReg f) op TInt v1 v2
            return (TInt, VReg f)
      (_,_) -> concatStrings (t1,v1) (t2,v2)
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
  _ -> throwError $ T.pack $  "debug: " ++ (show e)

updateBlockNum :: Integer -> GenM ()
updateBlockNum num = modify $ \s -> s {currBlock = num}

jumpAndOr :: Abs.Expr -> LLVMVal -> RelOp -> GenM LLVMTypeVal
jumpAndOr e2 v1 op = do
  startBlock <- gets currBlock
  f <- getFresh
  emit $ Cmp (VReg f) op TBool v1 (VInt 0)
  br1 <- getFresh
  br2 <- getFresh
  emit $ BrCond (TBool, VReg f) (TLabel, VLabel br1) (TLabel, VLabel br2)
  emit $ Label $ VLabel $ toInteger br1
  updateBlockNum br1
  (_, v2) <- genExp e2
  --
  b <- gets currBlock
  --
  emit $ Br (TLabel, VLabel br2)
  emit $ Label $ VLabel $ toInteger br2
  updateBlockNum br2
  res <- getFresh
  --
  -- emit $ Phi (VReg res) TBool [(v1, VLabel startBlock), (v2, VLabel br1)]
  emit $ Phi (VReg res) TBool [(v1, VLabel startBlock), (v2, VLabel b)]
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
  ii <- gets ins
  m <- gets lits
  let ins2 = ii ++ (reverse $ map genStrDecl $ Map.toList m)
  modify $ \s -> s {ins = ins2}

emit :: Instr -> GenM ()
emit i = do
  ii <- gets ins
  modify $ \s -> s {ins = i:ii}

-- STATEMENTS GENERATION -------------

mapType :: Abs.Type -> LLVMType
mapType t = case t of
        Abs.Int -> TInt
        Abs.Str -> TPtr TChar
        Abs.Bool -> TBool
        Abs.Void -> TVoid
        Abs.ClassType id -> TPtr $ TStructName id
        _ -> error "internal error mapType"

defaultVal :: LLVMType -> LLVMVal
defaultVal t = case t of
  TInt -> VInt 0
  TPtr TChar -> VGlobStr 0
  TBool -> VBool False
  TVoid -> VVoid
  TPtr (TStructName id) -> VNull

declChangeEnv :: (FuncEnv, VarEnv) -> (Abs.Type, Abs.Item) -> GenM (FuncEnv, VarEnv)
declChangeEnv (fenv,venv) (t, (Abs.NoInit id)) = do
  let val = defaultVal $ mapType t 
  return (fenv, Map.insert id (mapType t, val) venv)
declChangeEnv (fenv,venv) (_t, (Abs.Init id e)) = do
  let t = mapType _t 
  (_, v) <- genExp e
  return (fenv, Map.insert id (t, v) venv)

-- returns changed variable env, because
-- Reader's "local" function is not enough; sometimes we need to overwrite values from
-- outer block
-- inner - list of variables already declared in this block
-- outer - map of locally changed outer variables
genStmt :: Abs.Stmt -> ([Abs.Ident], Map.Map Abs.Ident LLVMTypeVal) -> GenM ([Abs.Ident], Map.Map Abs.Ident LLVMTypeVal)
genStmt s (inner, outer) = do
  fnv <- gets fenv
  vnv <- gets venv
  case s of
    Abs.Decl t items -> do
      let env = (fnv, vnv) 
      (fnv', vnv') <- foldM declChangeEnv env $ zip (repeat t) items
      modify \s -> s { fenv = fnv', venv = vnv'}
      return (inner ++ map (^.Abs.iid) items, outer)
    Abs.Ass left e -> do
      val <- genExp e
      case left of
        Abs.EVar id -> do
          case id `elem` inner of
            True -> do
              modify \s -> s { venv = Map.insert id val vnv }
              return (inner, outer)
            False -> do
              let vnv' = Map.insert id val vnv
              modify \s -> s { venv = Map.insert id val vnv }
              return (inner, Map.insert id val outer)
        Abs.EField (Abs.EVar clsVar) fieldId -> do
          cenv <- gets clsenv
          -- gets venv >>= traceM .show
          base@(TPtr (TStructName clsName), VReg n) <- getVar clsVar
          let (num,t) = (cenv Map.! clsName) Map.! fieldId
          -- bitcastField <- getFresh
          fieldLoc <- getFresh
          emit $ GetElemPtr (VReg fieldLoc) (TStructName clsName) [
            base,
            (TInt, VInt 0),
            (TInt, VInt num)]
          -- emit $ BitCast (VReg bitcastField) (TPtr $ TStructName clsName, VReg fieldLoc) (TPtr t)
          emit $ Store val $ (TPtr t, VReg fieldLoc)
          case clsVar `elem` inner of
            True -> return (inner, outer)
            False -> return (inner, outer)
        Abs.EField (Abs.EField e1 e2) fieldId -> throwError $ T.pack "multiple levels struct props referencing not required"
    Abs.BStmt b -> do
      out <- genBlock b
      modify \s -> s { venv = Map.union out vnv }
      return (inner, Map.union out outer)
    Abs.Decr id -> do
      val <- doAdd id (-1)
      let vnv' = Map.insert id val vnv
      case id `elem` inner of
        True -> do
          modify \s -> s { venv = vnv' }
          return (inner, outer)
        False -> do
          modify \s -> s { venv = vnv' }
          return (inner, Map.insert id val outer)
    Abs.Incr id -> do
      val <- doAdd id 1
      let vnv' = Map.insert id val vnv
      case id `elem` inner of
        True -> do
          modify \s -> s { venv = vnv' }
          return (inner, outer)
        False -> do
          modify \s -> s { venv = vnv' }
          return (inner, Map.insert id val outer)
    Abs.Empty -> return (inner, outer)
    Abs.Ret e -> do
      val <- genExp e
      emit $ Ret val
      return (inner, outer)
    Abs.VRet -> do
      emit $ Ret (TVoid, VDummy)
      return (inner, outer)
    Abs.SExp e -> do
      val <- genExp e
      return (inner, outer)
    Abs.CondElse e s1 s2 -> do
      (TBool,v) <- genExp e
      trueLabel <- getFresh
      falseLabel <- getFresh
      endLabel <- getFresh
      emit $ BrCond (TBool, v) (TLabel, VLabel trueLabel) (TLabel, VLabel falseLabel)

      emit $ Label $ VLabel $ toInteger trueLabel
      modify \s -> s { currBlock = trueLabel }
      genStmt s1 (inner, outer)
      emit $ Br (TLabel, VLabel endLabel)
      mm1 <- gets venv
      modify \s -> s { venv = vnv}

      emit $ Label $ VLabel $ toInteger falseLabel
      modify \s -> s { currBlock = falseLabel }
      genStmt s2 (inner, outer)
      emit $ Br (TLabel, VLabel endLabel)
      mm2 <- gets venv
      modify \s -> s { venv = vnv}

      let AssCheck _ ass1 = execState (varsAssigned s1) (AssCheck [] [])
      let AssCheck _ ass2 = execState (varsAssigned s2) (AssCheck [] [])
      let phiNodes = LU.unique $ L.union ass1 ass2
      phiTvs <- mapM getVar phiNodes
      
      _phiIns <- mapM (\((t,v), id) -> constructPhi mm1 mm2 (VLabel trueLabel) (VLabel falseLabel) (id,t,v)) $ zip phiTvs phiNodes
      let phiIns = map snd _phiIns
      let phiMap = Map.fromList $ map fst _phiIns

      emit $ Label $ VLabel endLabel
      forM_ phiIns emit
      
      modify \s -> s { currBlock = endLabel }
      modify \s -> s { venv = Map.union phiMap vnv }
      
      return (inner, Map.union phiMap outer)
    Abs.Cond e s1 -> do
      genStmt (Abs.CondElse e s1 Abs.Empty) (inner, outer)
    Abs.While e s@(Abs.BStmt _) -> do
      block0 <- gets currBlock
      let AssCheck _ _phiNodes = execState (varsAssigned s) $ AssCheck [] []
      let phiNodes = LU.unique $ _phiNodes
      phiTvs <- mapM getVar phiNodes
      let phiTs = map fst phiTvs
      
      startBlock <- getFresh
      emit $ Br (TLabel, VLabel startBlock)
      emit $ Label $ VLabel startBlock
      updateBlockNum startBlock

      iiStart <- gets ins
      modify \s -> s { ins = [] }


      let phiNum = length phiNodes
      phiStartNum <- gets fresh -- without modifying anything
      modify \s -> s { fresh = phiStartNum + (toInteger phiNum) }
      
      let phiMap = Map.fromList $ zip phiNodes $ zip phiTs $ map VReg [phiStartNum..]
      
      modify \s -> s { venv = Map.union phiMap vnv }
      val <- genExp e
      
      iiE <- gets ins
      modify \s -> s { ins = [] }

      sLabel <- getFresh
      updateBlockNum sLabel
      emit $ Label $ VLabel sLabel
      genStmt s (inner, outer)
      mm <- gets venv
      
      emit $ Br (TLabel, VLabel startBlock)
      iiS <- gets ins
      sEndLabel <- gets currBlock

      freshSave <- gets fresh
      modify \s -> s { fresh = phiStartNum }
      _phiIns <- mapM (\((t,v), id) -> constructPhi vnv mm (VLabel block0) (VLabel sEndLabel) (id,t,v)) $ zip phiTvs phiNodes
      modify \s -> s { fresh = freshSave }
      
      let phiIns = map snd _phiIns
      let phiMap = Map.fromList $ map fst _phiIns
      endLabel <- getFresh
      let allIns = concat [iiS,
                       [BrCond val (TLabel, VLabel sLabel) (TLabel, VLabel endLabel)],
                       iiE,
                       reverse phiIns,
                       iiStart]
      modify \s -> s { ins = allIns, currBlock = endLabel, venv = Map.union phiMap vnv}
      emit $ Label $ VLabel endLabel
      return (inner, Map.unions [phiMap, outer])
    Abs.While e s -> genStmt (Abs.While e (Abs.BStmt (Abs.Block [s]))) (inner, outer)


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
fixEmptyBlock rett (FunEnd:((Label l):rest)) = (FunEnd:(a:((Label l):rest)))
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
  vnv <- gets venv
  let (t, v) = vnv Map.! id
  case v of
    VInt n -> do
      return (t, VInt $ n + x)
    VReg n -> do
      f <- getFresh
      emit $ Bin (VReg f) Plus TInt (VReg n) (VInt x)
      return (t, (VReg f))


t0 :: Abs.Program -> FuncEnv
t0 (Abs.Program topDefs) = Map.union builtInFunctions $ Map.fromList $ map f (filter (is Abs._FnDef) topDefs)  where
  f = \(Abs.FnDef ret id args _) -> (id, TFun (mapType ret) (map mapType (map (^.Abs.t) args)))


createClsEnvMap :: [Abs.ClassDecl] -> Map.Map Abs.Ident (Integer, LLVMType)
createClsEnvMap ds = Map.fromList $ zip (map (^.Abs.cdid) ds) $ zip [0..] (map (mapType . (^.Abs.ct)) ds)

clsenv0 :: Abs.Program -> Map.Map Abs.Ident (Map.Map Abs.Ident (Integer, LLVMType))
clsenv0 (Abs.Program defs) = let cds = filter (is Abs._ClassDef) defs in
  Map.fromList $ zip (map (^.Abs.tid) cds) $ map createClsEnvMap $ map (^.Abs.cb.Abs.cdecls) cds


s0 :: Abs.Program -> GenS
s0 p = GenS [] 1 (Map.singleton (VGlobStr 0) "") 1 (t0 p) Map.empty (clsenv0 p)
  
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


createFunctionEnv :: [Abs.Arg] -> Map.Map Abs.Ident LLVMTypeVal
createFunctionEnv args = Map.fromList $ map f (zip [0..] args)
  where f = \(num, (Abs.Arg t id)) -> (id, (mapType t,VReg num))


genBlock :: Abs.Block -> GenM (Map.Map Abs.Ident LLVMTypeVal)
genBlock (Abs.Block ss) = do
  let comp = \(inner, outer) -> \s -> genStmt s (inner, outer)
  (_, res) <- foldM comp ([], Map.empty) ss
  return res

emitTopDefIR :: Abs.TopDef -> GenM ()
emitTopDefIR (Abs.FnDef ret (Abs.Ident id) args block) = do
  emit $ FunEntry id $ TFun (mapType ret) (map (mapType . (^.Abs.t)) args)
  let comp = genBlock block >> emit FunEnd
  emit $ Label $ VLabel $ toInteger $ length args
  modify $ \s -> s{ fresh = toInteger (1 + (length args)), currBlock = toInteger $ length args, venv = createFunctionEnv args}
  comp
  ii <- gets ins
  modify \s -> s {ins = fixEmptyBlock (mapType ret) ii}


emitStructDecls :: GenM ()
emitStructDecls = do
  cls <- gets clsenv
  let comp = \(idString, structType) -> emit $ StructDef idString structType
  let mySort = reverse . sortBy (flip compare `on` fst)
  let strT = \clsMap -> TStruct $ map snd $ mySort $ Map.elems clsMap
  flip forM_ comp $ map (bimap (^.Abs.idid) strT) (Map.toList cls)
   

emitProgramIR :: FilePath -> Abs.Program -> GenM T.Text
emitProgramIR fp (Abs.Program topDefs) = do
  let fun = filter (is Abs._FnDef) topDefs
  emitStructDecls
  forM_ fun emitTopDefIR
  emitGlobalStrDecls
  ii <- gets ins
  let decls = map (\(Abs.Ident a, b) -> Declare a b) $ Map.toList builtInFunctions
  let content = buildLines $ map show $ decls ++ (reverse ii)
  return $ buildIR fp content

runGenM :: Abs.Program -> GenM a -> Except T.Text a
runGenM p comp = evalStateT comp $ s0 p
  
runBackend :: FilePath -> Abs.Program -> Either T.Text T.Text
runBackend fp p = runExcept $ runGenM p (emitProgramIR fp p)
