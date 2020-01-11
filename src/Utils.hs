{-# LANGUAGE BlockArguments #-}

module Utils where

import qualified Data.Text as T
import qualified AbsLatte as Abs
import Control.Monad.State
import Control.Lens

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

buildText :: [T.Text] -> T.Text
buildText ts = T.intercalate (T.pack "\n") ts

buildLines :: [String] -> T.Text
buildLines xs = T.intercalate (T.pack "\n") (map T.pack xs) 
 
buildCommaString :: [String] -> String 
buildCommaString lst = T.unpack $ T.intercalate (T.pack ", ") $ map T.pack lst


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

