
{-# LANGUAGE BlockArguments #-}

module Backend where

import AbsLatte
import Types
import ParLatte
import SkelLatte
import PrintLatte
import SkelLatte
import LexLatte
import ErrM

import Frontend(StateM, itemIdent)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Frontend(runStateM)
import qualified Data.Map as Map
import qualified Data.Text as T

import Debug.Trace

type FuncType = (Type, [Type]) -- return, args
type TypeEnv = (Map.Map Ident FuncType, Map.Map (Block, Ident) Type)

createTypeEnvStmt :: Block -> Stmt -> StateM TypeEnv ()
createTypeEnvStmt b s = do
  (fenv, venv) <- get
  case s of
    BStmt b -> createTypeEnvBlock b
    Decl t items -> do
      let ids = map itemIdent items
      let venv' = Map.union (Map.fromList $ zip (zip (repeat b) ids) (repeat t)) venv
      put (fenv, venv')
    _ -> return ()

createTypeEnvBlock :: Block -> StateM TypeEnv ()
createTypeEnvBlock b@(Block stmts) = forM_ (zip (repeat b) stmts) (uncurry createTypeEnvStmt)

modifyVar = \f -> modify \(a, b) -> (a, f b)

createTypeEnvTopDef :: TopDef -> StateM TypeEnv ()
createTypeEnvTopDef (FnDef t id args b) = do
  let b0 = Map.fromList $ map (\(Arg t id) -> ((b, id), t)) args
  modifyVar $ const b0
  createTypeEnvBlock b

getFuncType :: TopDef -> FuncType
getFuncType (FnDef t _ args _) = (t, argTs) where
  argTs = map (\(Arg tt _) -> tt) args


createTypeEnv :: Program -> StateM TypeEnv ()
createTypeEnv (Program topDefs) = do
  let funcEnv = Map.fromList $ map (\a@(FnDef _ id _ _) -> (id, getFuncType a)) topDefs
  put (funcEnv, Map.empty)
  forM_ topDefs createTypeEnvTopDef


t0 :: TypeEnv
t0 = (Map.empty, Map.empty)

runBackend :: Program -> ExceptT T.Text IO ()
runBackend p@(Program topDefs) = do
  tenv <- evalStateT (createTypeEnv p) t0
  traceM $ show $ tenv
  return ()
