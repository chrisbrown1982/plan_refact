module IntroDef where

import Lang

introDef :: Id -> String -> Prog -> Prog
introDef id na (Prog id2 decs) =
  case introDecs id na decs of
    (decs', Just res) -> Prog id2 (decs' ++ [res])

introDecs :: Id -> String -> [(Id, Decl)] -> ([(Id, Decl)], Maybe (Id, Decl))
introDecs id na [] = ([], Nothing)
introDecs id2 na ((id, dec):rest) =
    case introMatch id2 na dec of
        (match', Just res) -> ((id, match') : rest, Just res)
        (match', Nothing) -> case introDecs id2 na rest of
                                (rest', res) -> ((id, match'):rest', res)

introMatch :: Id -> String -> Decl -> (Decl, Maybe (Id, Decl))
introMatch id2 na (Match id matches) =
    case introEqs id2 na matches of
        (matches', res) -> (Match id matches', res)

introEqs :: Id -> String -> [(Id, String, [Pat], Exp)]
                         -> ([(Id, String, [Pat], Exp)], Maybe (Id, Decl))
introEqs id2 na [] = ([], Nothing)
introEqs id2 na ((id, n, pats, e):rest)
 | inE id2 e = let (res, e') = introE id2 na e in ((id, n, pats, res) : rest, e')
 | otherwise = case introEqs id2 na rest of
                (rest', e') -> ((id, n, pats, e):rest', e')

inE :: Id -> Exp -> Bool
inE id (Lit id2 i) = id == id2
inE id (Var id2 s) = id == id2
inE id (Plus id2 e1 e2) = (id2 == id) || inE id e1 || inE id e2
inE id (App id2 e1 e2) = (id2 == id) || inE id e1 || inE id e2
inE id (Enum id2 s e) = id == id2

introE :: Id -> String -> Exp -> (Exp, Maybe (Id, Decl))
introE id n e@(Lit id2 i)
 | id == id2 = (App 99 (Var id n) e, Just (buildDef id n e))
introE id n e@(Var id2 i)
 | id == id2 = (App 99 (Var id n) e, Just (buildDef id n e))
introE id n e@(Plus id2 e1 e2)
 | id == id2 = (buildApp id n (freeVars e), Just (buildDef id n e))
 | inE id e1 = case introE id n e1 of
                (e1', e') -> (Plus id2 e1' e2, e')
 | inE id e2 = case introE id n e2 of
                (e2', e') -> (Plus id2 e1 e2', e')
introE id n e@(App id2 e1 e2)
 | id == id2 = (buildApp id n (freeVars e), Just (buildDef id n e))
 | inE id e1 = case introE id n e1 of
                (e1', e') -> (App id2 e1' e2, e')
 | inE id e2 = case introE id n e2 of
                (e2', e') -> (App id2 e1 e2', e')
introE id n e@(Enum id2 s e')
 | id == id2 = (App 99 (Var id n) e, Just (buildDef id n e))

prel :: [String]
prel = ["fold", "(+)"]

freeVars :: Exp -> [String]
freeVars (Lit _ _) = []
freeVars (Var _ s)
 | not $ elem s prel  = [s]
 | otherwise = []
freeVars (Plus _ e1 e2) = freeVars e1 ++ freeVars e2
freeVars (App _ e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Enum _ _ _) = []

buildApp :: Id -> String -> [String] -> Exp
buildApp id name = foldl (\b a -> App 99 b (Var 99 a)) (Var id name)

buildDef :: Id -> String -> Exp -> (Id, Decl)
buildDef id na e = (99, Match id [(id, na, buildPats (freeVars e), e)])

buildPats :: [String] -> [Pat]
buildPats = map (PVar 99)