module Types where

import qualified AbsLatte as Abs
import Utils

data Location =
  FunName Abs.Ident | 
  ClassMethod String String


instance Show Location where
  show what = case what of
    FunName (Abs.Ident id) -> "function " ++ id
    _ -> "TODO"



data BinOp =
  Plus
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
  TFun LLVMType [LLVMType] |
  TPtr LLVMType |
  TLabel |
  TStruct [LLVMType] |
  TStructName Abs.Ident
  deriving (Eq, Ord)

instance Show LLVMType where
  show t = case t of
    TInt -> "i32"
    TBool -> "i1"
    TVoid -> "void"
    TChar -> "i8"
    TPtr t -> (show t) ++ "*"
    TLabel -> "label"
    TArr num t -> "[" ++ (show num) ++ " x " ++ (show t) ++ "]"
    TFun ret args -> "<<TSfun, shouldn't be visible>>"
    TStruct ts -> "{ " ++ ( buildCommaString (map showw ts) )  ++ " }"
    TStructName (Abs.Ident s) -> "%struct." ++ s

data LLVMVal = VDummy
             | VInt Integer
             | VBool Bool
             | VVoid
             | VGlobStr Integer
             | VLabel Integer
             | VReg Integer
             | VStruct [LLVMVal]
             deriving (Eq, Ord)

instance Show LLVMVal where
  show val = case val of
    VInt n -> show n
    VBool b -> if b == True then "true" else "false"
    VVoid -> "void"
    VGlobStr n -> "@str." ++ show n
    VLabel n -> "L." ++ show n
    VReg n -> "%R." ++ show n
    VDummy -> ""


type LLVMTypeVal = (LLVMType, LLVMVal)
 
data Instr =
  GlobStrDecl LLVMVal String
  | FunEntry String LLVMType
  | Ret LLVMTypeVal
  | FunEnd
  | Bin LLVMVal BinOp LLVMType LLVMVal LLVMVal
  | Cmp LLVMVal RelOp LLVMType LLVMVal LLVMVal
  | FunCall LLVMVal LLVMType String [LLVMTypeVal]
  | GetElemPtr LLVMVal LLVMType [LLVMTypeVal]
  | Declare String LLVMType
  | BrCond LLVMTypeVal LLVMTypeVal LLVMTypeVal
  | Br LLVMTypeVal
  | Phi LLVMVal LLVMType [(LLVMVal, LLVMVal)]
  | Comment String
  | Label LLVMVal
  | StructDef String LLVMType
  | GetOffsetPtr LLVMVal LLVMType
  | PtrToInt LLVMVal LLVMTypeVal
  deriving (Eq, Ord)

instance Show Instr where
  show i = case i of
    FunEntry s (TFun ret args) -> "\ndefine " ++ (show ret) ++ "@" ++ s ++ "(" ++ (buildCommaString $ map showtv $ zip args $ map VReg [0..]) ++ ") {"
    FunEnd -> "}"
    GlobStrDecl n s -> (show n) ++ " = private unnamed_addr constant " ++ (show (TArr (toInteger (1 + (length s))) TChar)) ++ " c\"" ++ s ++ "\\00\", align 1" 
    Bin r op t v1 v2 -> (show r) ++ " = " ++ (show op) ++ " " ++ (show t) ++ " " ++ (show v1)  ++ ", " ++ (show v2)
    Cmp r op t v1 v2 -> (show r) ++ " = icmp " ++ (show op) ++ " " ++ (show t) ++ " " ++ (show v1)  ++ ", " ++ (show v2)
    Ret (t,v) -> "ret " ++ (show t) ++ " " ++ (show v)
    GetElemPtr r t tvs -> (show r) ++ " = getelementptr " ++ (show t) ++ ", " ++ (buildCommaString (map showtv tvs))
    FunCall r@(VReg _) t id args -> (show r) ++ " = call " ++ (show t) ++ " @" ++ id ++ "(" ++ (buildCommaString (map showtv  args)) ++ ")"
    FunCall VVoid TVoid id args -> "call " ++ (show VVoid) ++ " @" ++ id ++ "(" ++ (buildCommaString (map showtv args)) ++ ")"
    Declare funName (TFun ret args) -> "declare " ++ (show ret) ++ " @" ++ funName ++ "(" ++ (buildCommaString (map show args)) ++ ")"
    BrCond a@(condt, condv) b@(TLabel, l1V) c@(TLabel, l2v) -> "br " ++ (showtv a) ++ ", " ++ (buildCommaString (map showLab [b,c]))
    Br (t,v) -> "br " ++ (showLab (t,v))
    Phi v t lst -> (show v) ++ " = phi " ++ (show t) ++ " " ++ (buildCommaString (map phiShow lst))
    Comment s -> "; " ++ s
    Label n -> (show n) ++ ":"
    StructDef id t -> "%struct." ++ id ++ " = type " ++ (show t)
    GetOffsetPtr f@(VReg _) t@(TStructName _) -> (show f) ++ " = getelementptr " ++ (show t) ++ ", " ++ (show t) ++ "* null, i32 1" 
    PtrToInt x@(VReg _) (t@(TStructName _), y@(VReg _)) -> (show x) ++ " = ptrtoint " ++ (showw t) ++ " " ++ (show y) ++ " to i32"   


showw :: LLVMType -> String
showw t = case t of
  TStructName id -> (show t) ++ "*"
  _ -> show t

showtv :: LLVMTypeVal -> String
showtv (a,b) = (show a) ++ " " ++ (show b)

showLab :: LLVMTypeVal -> String
showLab (TLabel, vlab) = "label " ++ "%" ++ (show vlab)

phiShow :: (LLVMVal,LLVMVal) -> String
phiShow (v, l@(VLabel n)) = "[" ++ (show v) ++ ", %" ++ show l ++ "]"
