module Types where

import qualified AbsLatte as Abs

data Location =
  FunName Abs.Ident | 
  ClassMethod String String


instance Show Location where
  show what = case what of
    FunName (Abs.Ident id) -> "function " ++ id
    _ -> "TODO"





data AddOp = Plus | Minus
  deriving (Eq, Ord, Read)

instance Show AddOp where
  show s = case s of
    Plus -> "add"
    Minus -> "sub"

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Read)


instance Show MulOp where
  show s = case s of
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
  TArr Int LLVMType |
  TFun LLVMType [LLVMType] |
  TPtr LLVMType |
  TLabel
  deriving (Eq, Ord)

instance Show LLVMType where
  show t = case t of
    TInt -> "i32"
    TBool -> "i8"
    TVoid -> "void"
    TPtr t -> (show t) ++ "*"
    TLabel -> "labelTODO"
    TArr num t -> "[" ++ (show num) ++ ", " ++ (show t) ++ "]"
    TFun ret args -> "TODO"

data LLVMVal = VInt Integer
             | VBool Bool
             | VVoid
             | VGlobStr Integer
             | VLabel Integer
             | VReg Integer
             | VNull
             deriving (Eq, Ord)

instance Show LLVMVal where
  show val = case val of
    VInt n -> show n
    VBool b -> if b == True then show 1 else show 0
    VVoid -> "VOID TODO"
    VGlobStr n -> "@str." ++ show n
    VLabel n -> "TODO"
    VReg n -> "%R." ++ show n
    VNull -> "NULL TODO"

type LLVMTypeVal = (LLVMType, LLVMVal)


