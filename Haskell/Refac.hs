module Refac where

import Lang

generalise :: Prog -> Id -> Id -> String -> (Prog, Maybe Exp)
generalise (Prog id decs) fun_id id2 n = 
   case generaliseDecs id2 n decs of 
      (decs', Just res) -> (Prog id (changeCalls fun_id res decs'), Just res)
      (_, Nothing) -> error "something went wrong"

generaliseDecs :: Id -> String -> [(Id, Decl)] -> ([(Id, Decl)], Maybe Exp)
generaliseDecs  id n [] = ([], Nothing)
generaliseDecs  id2 n ((id, dec):rest) =
   case generaliseMatch id2 n dec of  -- should just do first occurrence?
      (match', res) -> case generaliseDecs id2 n rest of 
                         (rest', _) -> ((id, match') : rest', res) 

generaliseMatch :: Id -> String -> Decl -> (Decl, Maybe Exp)
generaliseMatch  id2 n (Match id matches) =
     case generaliseEq id2 n matches of 
        (matches', res) -> (Match id matches', res)

generaliseEq :: Id -> String ->  [(Id, String, [Pat], Exp)] -> ([(Id, String, [Pat], Exp)], Maybe Exp)
generaliseEq _ _  [] = ([], Nothing)
generaliseEq id2 n2 ((id, n, pats, e):rest)
   | inE id2 e  = let (res, e') = generaliseE id2 n2 e in ((id, n, PVar 99 n2:pats, res) :(modP (PVar 99 n2) rest), Just e')
   | otherwise = case generaliseEq id2 n2 rest of 
                   (rest', res) -> ((id, n, pats, e) : rest', res)

modP :: Pat -> [(Id, String, [Pat], Exp)] -> [(Id, String, [Pat], Exp)]
modP p [] = [] 
modP p ((id, n, pats, e):rest) = (id, n, p:pats,e ): modP p rest 

inE :: Id -> Exp -> Bool
inE id (Lit id2 i) = id == id2
inE id (Var id2 s) = id == id2
inE id (Plus id2 e1 e2) = (id2 == id) || inE id e1 || inE id e2
inE id (App id2 e1 e2) = (id2 == id) || inE id e1 || inE id e2
inE id (Enum id2 s e) = id == id2
inE id (Case m alts) = inAlts id alts

inAlts :: Id -> [(Pat, Exp)] -> Bool
inAlts id [] = False 
inAlts id ((p, e):rest) 
  | inE id e = True 
  | otherwise = inAlts id rest 

generaliseE :: Id -> String -> Exp -> (Exp, Exp) 
generaliseE id n e@(Lit id2 i)
   | id == id2 = (Var id2 n, e)
generaliseE id n v@(Var id2 v2)
   | id == id2 = (Var id2 n, v) -- this makes no sense
generaliseE id n e@(Plus id2 e1 e2)
   | id == id2 = (Var id2 n, e)
   | inE id e1 = case generaliseE id n e1 of 
                   (e1', e') -> (Plus id2 e1' e2, e')
   | inE id e2 = case generaliseE id n e2 of 
                   (e2', e') -> (Plus id2 e1 e2', e')
generaliseE id n e@(App id2 e1 e2)
   | id == id2 = (Var id2 n, e)
   | inE id e1 = case generaliseE id n e1 of
                  (e1', e') -> (App id2 e1' e2, e')
   | inE id e2 = case generaliseE id n e2 of
                  (e2', e') -> (App id2 e1 e2', e')
generaliseE id n e@(Enum id2 s e')
   | id == id2 = (Var id2 n, e)
generaliseE id n e@(Case m alts)
   | inAlts id alts 
       = case generaliseCase id n alts of
           (alts', e') -> (Case m alts', e')
generaliseE _ _ e = (e, e)

generaliseCase :: Id -> String -> [(Pat, Exp)] -> ([(Pat, Exp)], Exp)
generaliseCase id n [] = error "error in generalisation"
generaliseCase id n ((p,e):rest)
  | inE id e = case generaliseE id n e of 
                  (e', e'') -> ((p, e'):rest, e'')
  | otherwise = case generaliseCase id n rest of 
                  (rest', e') -> ((p,e):rest', e')

----

changeCalls :: Id -> Exp -> [(Id, Decl)] -> [(Id, Decl)]
changeCalls fun_id e [] = [] 
changeCalls fun_id e ((id,dec):decs) 
  = (id, changeCallsDecls fun_id e dec) : changeCalls fun_id e decs 

changeCallsDecls :: Id -> Exp -> Decl -> Decl 
changeCallsDecls fun_id e (Match id matches) 
  = Match id (changeCallsMatch fun_id e matches)

changeCallsMatch :: Id -> Exp -> [(Id, String, [Pat], Exp)] -> [(Id, String, [Pat], Exp)] 
changeCallsMatch fun_id e [] = []
changeCallsMatch fun_id e ((id, name, pats, e2):rest) 
  = (id, name, pats, changeCallsExp fun_id e e2):changeCallsMatch fun_id e rest 

changeCallsExp :: Id -> Exp -> Exp -> Exp 
changeCallsExp fun_id e a@(App id (Var id2 e1) e2) 
  | fun_id == id2 = App id (App 99 (Var id2 e1 ) e) e2 
  | otherwise = App id (Var id2 e1) (changeCallsExp fun_id e e2)
changeCallsExp fun_id e a@(App id e1 e2) = App id (changeCallsExp fun_id e e1) (changeCallsExp fun_id e e2)
changeCallsExp fun_id e a@(Lit id i) = a 
changeCallsExp fun_id e a@(Var id s) = a 
changeCallsExp fun_id e (Plus id e1 e2) = Plus id (changeCallsExp fun_id e e1) (changeCallsExp fun_id e e2)
changeCallsExp fun_id e a@(Enum id i1 i2) = a
changeCallsExp fun_id e a@(Case m alts) =
     Case (changeCallsExp fun_id e m) (changeCallsAlts fun_id e alts)
changeCallsExp _ _ e = error $ show e 

changeCallsAlts :: Id -> Exp -> [(Pat, Exp)] -> [(Pat, Exp)]
changeCallsAlts _ _ [] = []
changeCallsAlts fun_id e ((p,e1):rest)
  = (p, changeCallsExp fun_id e e1):changeCallsAlts fun_id e rest