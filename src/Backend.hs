{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Backend where

import qualified AbsLatte as Abs

import Types
import Utils
import System.FilePath

import Frontend(itemIdent)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fail
import Control.Lens

import qualified Data.Map as Map
import qualified Data.Text as T

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
  (_,num,_) <- get
  modify \(c, n, m) -> (c, n+1, m)
  return num



type GenE = (FuncEnv,
             Map.Map Abs.Ident LLVMTypeVal)
-- function env and variable env

type GenS = ([Instr],
             Integer,
             Map.Map LLVMVal String)
-- (code generated,
--  num of fresh variable,
--  Map for global string literals)


type GenM = ReaderT GenE (StateT GenS (Except T.Text))


instance MonadFail Identity where
    fail :: String -> m a
    fail = error -- return 1 -- throwError "Internal error: PatternMatching failed!?"

getVar :: Abs.Ident -> GenM LLVMTypeVal
getVar id = do
  (_, m) <- ask
  return $ m Map.! id

getFunType :: Abs.Ident -> GenM LLVMType
getFunType id = do
  (m, _) <- ask
  return $ m Map.! id


debug = (TInt, VInt 2137)

genExp :: Abs.Expr -> GenM LLVMTypeVal
genExp e = case e of
  Abs.EVar id -> getVar id
  Abs.ELitInt n -> return (TInt, VInt n)
  Abs.ELitTrue -> return (TBool, VBool True)
  Abs.ELitFalse -> return (TBool, VBool False)
  Abs.EApp id exps -> do
    tvs <- forM exps genExp
    TFun ret args <- getFunType id
    fresh <- getFresh
    emit $ FunCall (VReg fresh) ret tvs
    return (ret, VReg fresh)
  Abs.EString s -> do
    (ins,n,m) <- get -- global strings map
    let glob = VGlobStr $ toInteger $ Map.size m -- numbering strings from zero
    put (ins,n, Map.insert glob s m)
    fresh <- getFresh
    let tarr = (TArr (toInteger (1 + length s)) TChar) in emit $ GetElemPtr (VReg fresh) tarr [(TPtr tarr, glob),
                                                        (TInt, VInt 0),
                                                        (TInt, VInt 0)]
    return (TPtr TChar, VReg fresh)
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
      (VInt n, VInt m) -> return (TInt, VInt $ n + m)
      (VReg n, VInt m) -> do
        fresh <- getFresh
        let op = case _op of
              Abs.Times -> Times
              Abs.Div -> Div
              Abs.Mod -> Mod
        emit $ Bin (VReg fresh) op TInt v1 v2
        return (TInt, VReg fresh)
    
  _ -> undefined


genStrDecl = \((VGlobStr n), s) -> flip GlobStrDecl s $ VGlobStr $ toInteger $ n

emitGlobalStrDecls :: GenM ()
emitGlobalStrDecls = do
  (ins,n,m) <- get
  let ins2 = ins ++ (reverse $ map genStrDecl $ Map.toList m)
  put (ins2,n,m)

emit :: Instr -> GenM ()
emit i = do
  (ins,n,m) <- get
  put (i:ins,n,m)

t0 :: Abs.Program -> FuncEnv
t0 (Abs.Program topDefs) = Map.fromList $ map f topDefs  where
  f = \(Abs.FnDef ret id args _) -> (id, TFun (absTypeToLLVM ret) (map absTypeToLLVM (map (^.Abs.t) args)))
                                       

e0 :: Abs.Program -> GenE
e0 p = (t0 p, Map.empty)
      
s0 :: GenS
s0 = ([], 1, Map.empty)

emitProgramIR :: FilePath -> Abs.Program -> GenM T.Text
emitProgramIR fp p = do
  let funCode = map T.pack [] -- TODO
  return $ buildIR fp funCode


 
buildIR :: FilePath -> [T.Text] -> T.Text
buildIR filename funIRs = buildText [buildLines $ prolog (show filename),
                                     buildText funIRs,
                                     buildLines epilog]


runGenM :: Abs.Program -> GenM a -> Except T.Text a
runGenM p comp = evalStateT (runReaderT comp (e0 p)) s0
  
runBackend :: FilePath -> Abs.Program -> Either T.Text T.Text
runBackend fp p = runExcept $ runGenM p (emitProgramIR fp p)
