module Types where

import qualified AbsLatte as Abs

data Location =
  FunName Abs.Ident | 
  ClassMethod String String


instance Show Location where
  show what = case what of
    FunName (Abs.Ident id) -> "function " ++ id
    _ -> "TODO"



data BinOp = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Eq, Ord, Read)

instance Show BinOp where
  show s = case s of
    Plus -> "add"
    Minus -> "sub"
    Times -> "mul"
    Div -> "sdiv"
    Mod -> "srem"
    
data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Read)

instance Show RelOp where
  show s = case s of
    LTH -> "slt"
    LE  -> "sle"
    GTH -> "sgt"
    GE  -> "sge"
    EQU -> "eq"
    NE  -> "ne"



data LLVMType =
  TInt |
  TBool |
  TVoid |
  TArr Integer LLVMType |
  TChar |
  TString |
  TFun LLVMType [LLVMType] |
  TPtr LLVMType |
  TLabel
  deriving (Eq, Ord)

instance Show LLVMType where
  show t = case t of
    TInt -> "i32"
    TBool -> "i8"
    TVoid -> "void"
    TChar -> "i8"
    TPtr t -> (show t) ++ "*"
    TLabel -> "labelTODO"
    TArr num t -> "[" ++ (show num) ++ ", " ++ (show t) ++ "]"
    TFun ret args -> "TODO"

data LLVMVal = VDummy
             | VInt Integer
             | VBool Bool
             | VVoid
             | VGlobStr Integer
             | VLabel Integer
             | VReg Integer
             deriving (Eq, Ord)

instance Show LLVMVal where
  show val = case val of
    VInt n -> show n
    VBool b -> if b == True then "true" else "false"
    VVoid -> "VOID TODO"
    VGlobStr n -> "@str." ++ show n
    VLabel n -> "TODO"
    VReg n -> "%R." ++ show n
    VDummy -> "TODO nic"


type LLVMTypeVal = (LLVMType, LLVMVal)
 
data Instr =
  GlobStrDecl LLVMVal String
  | FunEntry String LLVMType
  | Ret LLVMTypeVal
  | FunEnd
  | Bin LLVMVal BinOp LLVMType LLVMVal LLVMVal
  | Cmp LLVMVal RelOp LLVMType LLVMVal LLVMVal
  | FunCall LLVMVal LLVMType [LLVMTypeVal]
  | GetElemPtr LLVMVal LLVMType [LLVMTypeVal]

instance Show Instr where
  show i = case i of
    GlobStrDecl n s -> "@.str." ++ (show n) ++ " = private unnamed_addr constant" ++ s ++ "\\00\""
    Bin r op t v1 v2 -> (show r) ++ " = " ++ (show op) ++ " " ++ (show t) ++ " " ++ (show v1)  ++ ", " ++ (show v2)
    Cmp r op t v1 v2 -> (show r) ++ " = icmp " ++ (show op) ++ " " ++ (show t) ++ (show v1)  ++ ", " ++ (show v2)
      



