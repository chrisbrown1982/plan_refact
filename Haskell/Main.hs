module Main where

import Lang

import Refac
import Rename
import IntroDef 

import System.Environment (getArgs)

data RefacName = Generalise Id Id String | Rename Id String | IntroDef Id String | Pretty 
                    deriving (Show, Read)


callRefac :: Prog -> RefacName -> Prog
callRefac prog (Generalise id1 id2 n) = fst $ generalise prog id1 id2 n
callRefac prog (Rename id n) = rename id n prog 
callRefac prog (IntroDef id n) = introDef id n prog

main :: IO () 
main = do
        iPath:oPath:args <- getArgs
        putStrLn $ show (iPath:oPath:args)
        let r = (foldr (\x y -> x ++ " " ++ y) "" args) 
        putStrLn $ ">" ++ (init r) ++ "<"
        let argsParsed = read (init r) :: RefacName
--        f <- readFile pathToMainFile 
--        let p = read f :: Prog 
        putStrLn $ show (iPath, argsParsed)
        f <- readFile iPath
        let prog = read f :: Prog
        let prog' = callRefac prog argsParsed
        writeFile oPath (show prog')
        writeFile (oPath++"_pretty")  (pprintProg prog')
