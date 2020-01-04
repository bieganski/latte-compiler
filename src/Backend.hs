
{-# LANGUAGE BlockArguments #-}

module Backend where

import qualified AbsLatte as Abs
import Types

import System.FilePath

import Frontend(itemIdent)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Control.Lens

import qualified Data.Map as Map
import qualified Data.Text as T

import Debug.Trace
import Utils



type FuncEnv = Map.Map Abs.Ident LLVMType

absTypeToLLVM :: Abs.Type -> LLVMType
absTypeToLLVM t = case t of
            Abs.Int -> TInt
            Abs.Str -> TPtr TChar
            Abs.Bool -> TBool
            Abs.Void -> TVoid
            _ -> error "not implemented (absTypeToLLVM)"

getFresh :: GenM Int
getFresh = do
  (_,num,_) <- get
  modify \(c, num, m) -> (c, num+1, m)
  return num



type GenE = (FuncEnv,
             Map.Map Abs.Ident LLVMTypeVal)
-- function env and variable env

type GenS = ([Instr],
             Int,
             Map.Map Abs.Ident String)
-- (code generated,
--  num of fresh variable,
--  Map for global string literals)


type GenM = ReaderT GenE (StateT GenS (Except T.Text))


data Instr =
  GlobStrDecl Integer String
  | FunEntry String LLVMType
  | Ret LLVMTypeVal
  | FunEnd
  | Bin
  | FunCall LLVMVal LLVMType [LLVMTypeVal]

instance Show Instr where
  show i = case i of
    GlobStrDecl n s -> "@.str." ++ (show n) ++ " = private unnamed_addr constant"
      ++ s ++ "\\00\""


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
    vals <- forM exps genExp
    return $ debug
  _ -> undefined



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
