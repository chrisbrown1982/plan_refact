module Example where

import Lang
    ( Prog(..),
      Pat(PVar, Nil, Cons),
      Decl(Match),
      Exp(Enum, Lit, App, Var),
      pprintProg )
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

-- step 4 introduce new def for fold (+) .. 
step4 = introDef 99 "sum" step3