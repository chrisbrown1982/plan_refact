module Example where

import Lang

import Refac
import Rename
import IntroDef

example3 :: Prog 
example3 = Prog 1 decs 
 where 
   decs = [(2, dec1), (3, dec2)]
   dec1 = Match 5 [(6, "sum", [Nil 66], Lit 7 0), (6, "sum", [Cons 61 (PVar 7 "h") (PVar 8 "t")], App 9 (App 10 (Var 11 "(+)") (Var 12 "h")) (App 13 (Var 6 "sum") (Var 8 "t")))]

   dec2 = Match 14 [(15, "main", [], App 16 (Var 6 "sum") (Enum 17 1 4))]
   
-- step 1 generalise 0
step1 = fst $ generalise example3 6 7 "n"

step1Print = putStr $ pprintProg $ fst $ generalise example3 6 7 "n"

--step 2 generalise (+) to c 
-- Bugs: doesn't add a paramter to all equations if expression found after first equation
-- doesn't change recursive calls properly
step2 = fst $ generalise step1 6 11 "c"

-- step 3 rename sum to fold
step3 = rename 6 "fold" step2

step3a = Prog 1 [(2,Match 5 [(6,"fold",[PVar 99 "n",Nil 66],Var 7 "n"),
                   (6,"fold",[PVar 99 "c",PVar 99 "n",Cons 61 (PVar 7 "h") (PVar 8 "t")], 
                                  App 9 (App 10 (Var 11 "c") (Var 12 "h")) (App 13 (App 99 (App 99 (Var 6 "fold") (Var 11 "(+)")) (Lit 7 0)) (Var 8 "t")))]),
                                  (3,Match 14 [(15,"main",[],App 16 (App 999 (App 99 (Var 6 "fold") (Var 11 "(+)")) (Lit 7 0)) (Enum 17 1 4))])]


-- step 4 introduce new def for fold (+) .. 
step4 = introDef 999 "sum" step3a


-- with CASE

example3C :: Prog 
example3C = Prog 1 decs 
 where 
   decs = [(2, dec1), (3, dec2)]

   dec1 = Match 5 [(6, "sum", [PVar 66 "x"], 
                   Case (Var 66 "x") 
                     [
                       (Nil 666, Lit 7 0),
                       (Cons 61 (PVar 7 "h") (PVar 8 "t"), App 9 (App 10 (Var 11 "(+)") (Var 12 "h")) (App 13 (Var 6 "sum") (Var 8 "t")))  
                     ])]

   dec2 = Match 14 [(15, "main", [], App 16 (Var 6 "sum") (Enum 17 1 4))]


step1C = fst $ generalise example3C 6 7 "n"

step2C = fst $ generalise step1C 6 11 "c"

step3C = rename 6 "fold" step2C

step3CA = Prog 1 [(2,Match 5 [(6,"fold",[PVar 99 "c",PVar 99 "n",PVar 66 "x"],Case (Var 66 "x") [(Nil 666,Var 7 "n"),(Cons 61 (PVar 7 "h") (PVar 8 "t"),App 9 (App 10 (Var 11 "c") (Var 12 "h")) (App 13 (App 99 (App 99 (Var 6 "fold") (Var 11 "(+)")) (Lit 7 0)) (Var 8 "t")))])]),(3,Match 14 [(15,"main",[],App 16 (App 999 (App 99 (Var 6 "fold") (Var 11 "(+)")) (Lit 7 0)) (Enum 17 1 4))])]

step4C =  introDef 999 "sum" step3CA