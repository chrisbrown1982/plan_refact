module Lang where

type Id = Int

data Exp = 
     Lit Id Int
   | Var Id String
   | Plus Id Exp Exp 
   | App Id Exp Exp 
   | Enum Id Int Int 

data Decl = 
   Match Id [(Id, String, [Pat], Exp)]


data Pat = 
    Nil Id 
  | Cons Id Pat Pat 
  | PVar Id String 

data Prog = Prog Id [(Id, Decl)]

pprintE :: Exp -> String 
pprintE (Lit _ i) = show i 
pprintE (Var _ s) = s 
pprintE (Plus _ e1 e2) = pprintE e1 ++ " + " ++ pprintE e2
pprintE (App _ e1 e2) = pprintE e1 ++ " " ++ pprintE e2
pprintE (Enum _ s e) = "[" ++ (show s) ++ ".." ++ (show e) ++ "]"

pprintD :: (Decl) -> String 
pprintD (Match _ []) = ""
pprintD (Match _ matches) = pprintMatches matches 

pprintMatches :: [(Id, String, [Pat], Exp)] -> String 
pprintMatches [] = ""
pprintMatches ((_, n, pats, e):rest) = n ++ (concat(map pprintPat pats)) ++ " = " ++ pprintE e ++ "\n"

pprintPat :: Pat -> String 
pprintPat (Nil _) = "[]"
pprintPat (Cons _ x xs) = "(" ++ pprintPat x ++ ":" ++ pprintPat xs ++ ")"
pprintPat (PVar _ s) = s 

pprintProg :: Prog -> String 
pprintProg (Prog _ []) = ""
pprintProg (Prog _ ds) = concat $ map (pprintD . snd) ds