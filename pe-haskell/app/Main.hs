-- Main.hs
-- Project Euler CLI

-- Using the Haskell intermpreter hint package.


import Data.Char (toUpper)
import Data.List (intercalate)
import System.Environment (getArgs)

import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do
  args <- getArgs
  let funcName = head args
  let modName = upCase funcName

  let projectEuler = do
        say $ "Running " ++ funcName ++ " ..."
        loadModules ["src/" ++ modName]
        setImportsQ [(modName, Just "Mpe")]
        runStmt $ "Mpe." ++ funcName
--        setImportsF [ModuleImport modName NotQualified (ImportList [funcName])]
--        runStmt funcName
        
  r <- runInterpreter projectEuler
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

upCase :: String -> String
upCase (x:xs) = toUpper x : xs

