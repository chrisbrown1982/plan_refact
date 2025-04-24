module Refac where

import Lang

generalise :: Prog -> Id -> String -> Prog 
generalise (Prog id decs) id2 n = Prog id (generaliseDecs id2 n decs)

generaliseDecs :: Id -> String -> [(Id, Decl)] -> [(Id, Decl)]
generaliseDecs  id n [] = [] 
generaliseDecs  id2 n ((id, dec):rest) =
 (id, generaliseMatch id2 n dec) : generaliseDecs id2 n rest 

generaliseMatch :: Id -> String -> Decl -> Decl 
generaliseMatch  id2 n (Match id matches) =
    Match id (generaliseEq id2 n matches)

generaliseEq :: Id -> String ->  [(Id, String, [Pat], Exp)] -> [(Id, String, [Pat], Exp)]
generaliseEq _ _  [] = [] 
generaliseEq id2 n2 ((id, n, pats, e):rest)
   | inE id e  = ((id, n, (PVar 99 n2):pats, generaliseE id n e):rest)
   | otherwise = generaliseEq id2 n2 rest

inE :: Id -> Exp -> Bool
inE id (Lit id2 i) = id == id2 
inE id (Var id2 s) = id == id2 
inE id (Plus id2 e1 e2) = or [id2 == id, inE id e1, inE id e2]
inE id (App id2 e1 e2) = or [id2 == id, inE id e1, inE id e2]
inE id (Enum id2 s e) = id == id2

generaliseE :: Id -> String -> Exp -> Exp 
generaliseE id n (Lit id2 i)
   | id == id2 = Var id2 n 
generaliseE id n v@(Var id2 v2)
   | id == id2 = v -- this makes no sense
generaliseE id n (Plus id2 e1 e2) 
   | id == id2 = Var id2 n 
   | inE id e1 = Plus id2 (generaliseE id n e1) e2 
   | inE id e2 = Plus id2 e1 (generaliseE id n e2)
generaliseE id n (App id2 e1 e2)
   | id == id2 = Var id2 n 
   | inE id e1 = App id2 (generaliseE id n e1) e2 
   | inE id e2 = App id2 e1 (generaliseE id n e2)
generaliseE id n (Enum id2 s e) 
   | id == id2 = Var id2 n
generaliseE _ _ e = e