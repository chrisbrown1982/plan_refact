module Lang where

type Id = Int

data Exp = 
     Lit Id Int
   | Var Id String
   | Plus Id Exp Exp 
   | App Id Exp Exp 
   | Enum Id Int Int 
   | Case Exp [(Pat, Exp)]
  deriving Show 

data Decl = 
   Match Id [(Id, String, [Pat], Exp)]
  deriving Show 

data Pat = 
    Nil Id 
  | Cons Id Pat Pat 
  | PVar Id String 
 deriving Show 

data Prog = Prog Id [(Id, Decl)] deriving Show 

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


