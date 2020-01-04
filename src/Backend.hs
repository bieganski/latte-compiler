
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

type FuncType = (Abs.Type, [Abs.Type]) -- return, args
type TypeEnv = (Map.Map Abs.Ident FuncType, Map.Map (Abs.Block, Abs.Ident) Abs.Type)


createTypeEnvStmt :: Abs.Block -> Abs.Stmt -> State TypeEnv ()
createTypeEnvStmt b s = do
  (fenv, venv) <- get
  case s of
    Abs.BStmt b -> createTypeEnvBlock b
    Abs.Decl t items -> do
      let ids = map (^.Abs.iid) items
      let venv' = Map.union (Map.fromList $ zip (zip (repeat b) ids) (repeat t)) venv
      put (fenv, venv')
    _ -> return ()

createTypeEnvBlock :: Abs.Block -> State TypeEnv ()
createTypeEnvBlock b@(Abs.Block stmts) = forM_ (zip (repeat b) stmts) (uncurry createTypeEnvStmt)

modifyVar = \f -> modify \(a, b) -> (a, f b)

createTypeEnvTopDef :: Abs.TopDef -> State TypeEnv ()
createTypeEnvTopDef (Abs.FnDef t id args b) = do
  let b0 = Map.fromList $ map (\(Abs.Arg t id) -> ((b, id), t)) args
  modifyVar $ const b0
  createTypeEnvBlock b

getFuncType :: Abs.TopDef -> FuncType
getFuncType (Abs.FnDef t _ args _) = (t, argTs) where
  argTs = map (\(Abs.Arg tt _) -> tt) args


createTypeEnv :: Abs.Program -> State TypeEnv TypeEnv
createTypeEnv (Abs.Program topDefs) = do
  let funcEnv = Map.fromList $ map (\a@(Abs.FnDef _ id _ _) -> (id, getFuncType a)) topDefs
  put (funcEnv, Map.empty)
  forM_ topDefs createTypeEnvTopDef
  get >>= return


prolog :: String -> [String]
prolog fname = ["source_filename = " ++ fname,
                "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"",
          "target triple = \"x86_64-pc-linux-gnu\"\n"]

epilog = ["attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "attributes #1 = { \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "!llvm.module.flags = !{!0}",
          "!llvm.ident = !{!1}\n",
          "!0 = !{i32 1, !\"wchar_size\", i32 4}",
          "!1 = !{!\"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)\"}"]

main_begin = ["define i32 @main() #0 {"]
main_end = ["ret i32 0", "}"]

buildText :: [T.Text] -> T.Text
buildText ts = T.intercalate (T.pack "\n") ts

buildLines :: [String] -> T.Text
buildLines xs = T.intercalate (T.pack "\n") (map T.pack xs) 
 
buildCommaString :: [String] -> String 
buildCommaString lst = T.unpack $ T.intercalate (T.pack ",") $ map T.pack lst

 
buildIR :: FilePath -> [T.Text] -> T.Text
buildIR filename funIRs = buildText [buildLines $ prolog (show filename),
                                     buildText funIRs,
                                     buildLines epilog]


getFresh :: GenM Int
getFresh = do
  (_,num,_,_) <- get
  modify \(c, num, t, m) -> (c, num+1, t, m)
  return num



type GenE = Map.Map Abs.Ident LLVMTypeVal
type GenS = ([Instr], Int, TypeEnv, Map.Map Abs.Ident String)
-- (code generated, num of fresh variable, type env, Map for global string literals)


type GenM = ReaderT GenE (StateT GenS (Except T.Text))


data Instr =
  GlobStrDecl Integer String
  | FunEntry String LLVMType
  | Ret LLVMTypeVal
  | FunEnd
  | Bin
  | FunCall LLVMValue LLVMType [LLVMTypeVal]

instance Show Instr where
  show i = case i of
    GlobStrDecl n s -> "@.str." ++ (show n) ++ " = private unnamed_addr constant"
      ++ s ++ "\\00\""


getVar :: Abs.Ident -> GenM LLVMTypeVal
getVar id = do
  m <- ask
  return $ m Map.! id

genExp :: Abs.Expr -> GenM LLVMTypeVal
genExp e = case e of
  Abs.EVar id -> getVar id
  Abs.ELitInt n -> return (TInt, VInt n)
  Abs.ELitTrue -> return (TBool, VBool True)
  Abs.ELitFalse -> return (TBool, VBool False)
  Abs.EApp id exps -> do
    vals <- forM exps genExp
    return $ FunCall
  _ -> undefined


t0 :: TypeEnv
t0 = (Map.empty, Map.empty)


emitProgramIR :: FilePath -> Abs.Program -> GenM T.Text
emitProgramIR fp p = do
  let funCode = map T.pack [] -- TODO
  return $ buildIR fp funCode

runGenM :: Abs.Program -> GenM a -> Except T.Text a
runGenM p comp = evalStateT (runReaderT comp Map.empty) ([], 0, tenv, Map.empty)
  where tenv = execState (createTypeEnv p) t0
  
runBackend :: FilePath -> Abs.Program -> Either T.Text T.Text
runBackend fp p = runExcept $ runGenM p (emitProgramIR fp p)
