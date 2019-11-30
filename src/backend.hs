module Backend where


-- TODO, mozliwe ze FuncType trzeba bedize importowac z frontendu
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
