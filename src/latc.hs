import AbsInstant
import ParInstant
import SkelInstant
import PrintInstant
import SkelInstant
import LexInstant
import ErrM

-- bnfc stuff
type ParseFun a = [Token] -> Err a
myLLexer = myLexer
type Verbosity = Int








runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f

run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  case ts of
           Bad s    -> putStrLn "\nParse failed...\n"
           Ok  tree -> do
             let ir = T.unpack $ buildIR tree
             let outLL = dropExtension fp <.> "ll"
             writeFile outLL ir
             readProcess "llvm-as" [outLL] ""
             return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _ -> error "args error!"

