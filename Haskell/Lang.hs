module Lang where

type Id = Int

data Exp = 
     Lit Id Int
   | Var Id String
   | Plus Id Exp Exp 
   | App Id Exp Exp 
   | Enum Id Int Int 
   | Case Exp [(Pat, Exp)]
  deriving (Read, Show) 

data Decl = 
   Match Id [(Id, String, [Pat], Exp)]
  deriving (Read, Show) 

data Pat = 
    Nil Id 
  | Cons Id Pat Pat 
  | PVar Id String 
 deriving (Read, Show) 

data Prog = Prog Id [(Id, Decl)] deriving (Read, Show) 


collectIDsM :: [(Pat, Exp)] -> [Id]
collectIDsM [] = []
collectIDsM ((p,e):ms) = collectIDsPats [p] ++ collectIDsExp e ++ collectIDsM ms

collectIDsExp :: Exp -> [ Id ]
collectIDsExp (Lit id _) = [id]
collectIDsExp (Var id _) = [id]
collectIDsExp (Plus id e1 e2) = id: (collectIDsExp e1 ++ collectIDsExp e2)
collectIDsExp (App id e1 e2) = id: (collectIDsExp e1 ++ collectIDsExp e2)
collectIDsExp (Enum id _ _) = [id]
collectIDsExp (Case e m) = collectIDsExp e ++ collectIDsM m 

uniqueIDExp :: Exp -> Id
uniqueIDExp e = (maximum $ collectIDsExp e) + 1


collectIDsPats :: [ Pat ] -> [ Id ]
collectIDsPats [] = [] 
collectIDsPats (Nil id:pats) = id : collectIDsPats pats 
collectIDsPats (Cons id p1 p2:pats) = id : (collectIDsPats [p1] ++ collectIDsPats [p2] ++ collectIDsPats pats) 
collectIDsPats (PVar id _:pats) = id : collectIDsPats pats

uniqueIDPats :: [ Pat ] -> Id 
uniqueIDPats p = (maximum $ collectIDsPats p) + 1

collectIDsMatches :: [(Id, String, [Pat], Exp)] -> [ Id ]
collectIDsMatches [] = [] 
collectIDsMatches ((id, _, pats, e):matches) = id: (collectIDsPats pats ++ collectIDsExp e ++ collectIDsMatches matches)

uniqueIDMatches :: [(Id, String, [Pat], Exp)] -> Id 
uniqueIDMatches ms = (maximum $ collectIDsMatches ms) + 1

collectIDsDecl :: Decl -> [ Id ]
collectIDsDecl (Match id matches) = id:collectIDsMatches matches

uniqueIDDecl :: Decl -> Id 
uniqueIDDecl d = (maximum $ collectIDsDecl d) + 1


collectIDsDecs :: [ (Id, Decl)] -> [ Id ]
collectIDsDecs [] = []
collectIDsDecs ((id, d):decs) = id : (collectIDsDecl d ++ collectIDsDecs decs)

collectIDsP :: Prog -> [ Id ]
collectIDsP (Prog id decs) = id : collectIDsDecs decs

uniqueIDProg :: Prog -> Id 
uniqueIDProg p = (maximum $ collectIDsP p) + 1

pprintE :: Exp -> String 
pprintE (Lit _ i) = show i 
pprintE (Var _ s) = s 
pprintE (Plus _ e1 e2) = "(" ++ pprintE e1 ++ " + " ++ pprintE e2 ++ ")"
pprintE (App _ e1 e2) = "(" ++ pprintE e1 ++ " " ++ pprintE e2 ++ ")"
pprintE (Enum _ s e) = "[" ++ (show s) ++ ".." ++ (show e) ++ "]"
pprintE (Case m alts) = "case " ++ pprintE m++ " of \n" ++ pprintAlts alts

pprintAlts :: [(Pat, Exp)] -> String 
pprintAlts [] = ""
pprintAlts ((p, e):rest)
  = "\t" ++ pprintPat p ++ " -> " ++ pprintE e ++ "\n" ++ pprintAlts rest

pprintD :: (Decl) -> String 
pprintD (Match _ []) = ""
pprintD (Match _ matches) = pprintMatches matches 

pprintMatches :: [(Id, String, [Pat], Exp)] -> String 
pprintMatches [] = ""
pprintMatches ((_, n, pats, e):rest) = n ++ " " ++ (concat(map pprintPat pats)) ++ " = " ++ pprintE e ++ "\n" ++ pprintMatches rest

pprintPat :: Pat -> String 
pprintPat (Nil _) = "[]"
pprintPat (Cons _ x xs) = "(" ++ pprintPat x ++ ":" ++ pprintPat xs ++ ")"
pprintPat (PVar _ s) = s ++ " " 

pprintProg :: Prog -> String 
pprintProg (Prog _ []) = ""
pprintProg (Prog _ ds) = concat $ map (pprintD . snd) ds


example1 :: Prog 
example1 = Prog 1 decs 
 where 
  decs = [(2, dec)]
  dec  = Match 3 [(4, "f", [PVar 5 "x"], Plus 6 (Var 7 "y") (Lit 8 1))]


example2 :: Prog 
example2 = Prog 1 decs 
 where 
  decs = [(2, dec), (9, dec2)]
  dec  = Match 3 [(4, "f", [PVar 5 "x"], Plus 6 (Var 7 "y") (Lit 8 1))]
  dec2 = Match 10 [(11, "g", [PVar 12 "x"], App 13 (Var 4 "f") (Lit 14 42))]


