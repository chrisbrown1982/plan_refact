module Rename where

import Lang 

rename :: Id -> String -> Prog -> Prog 
rename id na (Prog i prg) = Prog i (renameDecs id na prg)

renameDecs :: Id -> String -> [(Id, Decl)] -> [(Id, Decl)]
renameDecs id na [] = []
renameDecs id na ((i, dec):rest) =
 (i, renameMatches id na dec):renameDecs id na rest 

renameMatches :: Id -> String -> Decl -> Decl 
renameMatches id n (Match ii eqs)
 = Match ii (renameEqs id n eqs )

renameEqs :: Id -> String -> [(Id, String, [Pat], Exp)] -> [(Id, String, [Pat], Exp)]
renameEqs id n [] = [] 
renameEqs id na ((i, n2, pats, e):rest)
  | id == i = (i, na, pats, renameExp id na e):renameEqs id na rest 
  | otherwise = (i, n2, pats, renameExp id na e):renameEqs id na rest

renameExp :: Id -> String -> Exp -> Exp 
renameExp id na (Var id2 na2)
 | id == id2 = Var id2 na 
 | otherwise = Var id2 na2 
renameExp id na (Plus id2 e1 e2) = Plus id2 (renameExp id na e1) (renameExp id na e2)
renameExp id na (App id2 e1 e2) = App id2 (renameExp id na e1) (renameExp id na e2)
renameExp id na (Case e1 alts) = Case (renameExp id na e1) (renameAlts id na alts)
renameExp id na e = e 

renameAlts :: Id -> String -> [(Pat, Exp)] -> [(Pat, Exp)]
renameAlts _ _ [] = []
renameAlts id na ((p,e):rest) 
 = (p, renameExp id na e):renameAlts id na rest