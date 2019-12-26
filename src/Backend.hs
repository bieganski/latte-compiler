
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

import Control.Lens

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
      let ids = map (^.iid) items
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

runBackend :: Program -> ExceptT T.Text IO T.Text
runBackend p@(Program topDefs) = do
  tenv <- evalStateT (createTypeEnv p) t0
  return $ T.pack $ show tenv
  -- traceM $ show $ tenv
  -- return ()

prolog :: String -> [String]
prolog fname = ["source_filename = " ++ "\"" ++ fname ++ "\"",
          "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"",
          "target triple = \"x86_64-pc-linux-gnu\"\n",
          "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n"]

epilog = ["declare i32 @printf(i8*, ...) #1",
          "attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
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

typeToIRSize :: Type -> String
typeToIRSize t = case t of
  Int -> "i32"
  Str -> "i8*"
  Bool -> "i8"
  Void -> error "cannot typeToIRSIze for Void"
  Fun _ _ -> error "cannot typeToIRSIze for Fun"

 
buildCommaString :: [String] -> String 
buildCommaString lst = T.unpack $ T.intercalate (T.pack ",") $ map T.pack lst

buildFunctionIR :: String -> Type -> T.Text -> T.Text
buildFunctionIR funName (Fun ret args)  content = buildText
  [T.pack begin,
   content,
   T.pack "}"] where begin = "define " ++ (typeToIRSize ret) ++ "@" ++ funName ++ "("
                       ++ (buildCommaString (map typeToIRSize args)) ++ ")" ++ "#0 {"



