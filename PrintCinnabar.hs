{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintCinnabar where

-- pretty-printer generated by the BNF converter

import AbsCinnabar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Prog stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print Block where
  prt i e = case e of
    SBlock stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print Stmt where
  prt i e = case e of
    SWhile expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    SCond expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    SCondElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    SAssing lval expr -> prPrec i 1 (concatD [prt 0 lval, doc (showString "="), prt 0 expr])
    SReturn expr -> prPrec i 1 (concatD [doc (showString "return"), prt 0 expr])
    SPrint expr -> prPrec i 1 (concatD [doc (showString "print"), prt 0 expr])
    SAssert expr -> prPrec i 1 (concatD [doc (showString "assert"), prt 0 expr])
    SExpr expr -> prPrec i 1 (concatD [prt 0 expr])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print LVal where
  prt i e = case e of
    ATuple lvals -> prPrec i 0 (concatD [doc (showString "{"), prt 1 lvals, doc (showString "}")])
    AAt expr1 expr2 -> prPrec i 1 (concatD [prt 8 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    AMember expr id -> prPrec i 1 (concatD [prt 8 expr, doc (showString "."), prt 0 id])
    AVar id -> prPrec i 1 (concatD [prt 0 id])
  prtList 1 [x] = (concatD [prt 1 x])
  prtList 1 (x:xs) = (concatD [prt 1 x, doc (showString ","), prt 1 xs])
instance Print Expr where
  prt i e = case e of
    ELambda ids expr -> prPrec i 0 (concatD [doc (showString "lambda"), prt 0 ids, doc (showString ":"), prt 0 expr])
    EFun ids block -> prPrec i 0 (concatD [doc (showString "fun"), doc (showString "("), prt 0 ids, doc (showString ")"), prt 0 block])
    EIf expr1 expr2 expr3 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "if"), prt 0 expr2, doc (showString "else"), prt 0 expr3])
    EOr expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "||"), prt 1 expr2])
    EAnd expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "&&"), prt 2 expr2])
    ERel expr1 relop expr2 -> prPrec i 3 (concatD [prt 4 expr1, prt 0 relop, prt 3 expr2])
    EAdd expr1 addop expr2 -> prPrec i 4 (concatD [prt 5 expr1, prt 0 addop, prt 4 expr2])
    EMul expr1 mulop expr2 -> prPrec i 5 (concatD [prt 6 expr1, prt 0 mulop, prt 5 expr2])
    EPow expr1 expr2 -> prPrec i 6 (concatD [prt 7 expr1, doc (showString "^"), prt 6 expr2])
    ENot expr -> prPrec i 7 (concatD [doc (showString "!"), prt 7 expr])
    ENeg expr -> prPrec i 7 (concatD [doc (showString "-"), prt 7 expr])
    ECall expr exprs -> prPrec i 8 (concatD [prt 8 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EMember expr id -> prPrec i 8 (concatD [prt 8 expr, doc (showString "."), prt 0 id])
    EAt expr1 expr2 -> prPrec i 8 (concatD [prt 8 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    EExtend expr1 expr2 -> prPrec i 9 (concatD [doc (showString "extend"), prt 0 expr1, doc (showString "with"), prt 9 expr2])
    ENew expr exprs -> prPrec i 9 (concatD [doc (showString "new"), prt 9 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EString str -> prPrec i 10 (concatD [prt 0 str])
    ELitInt n -> prPrec i 10 (concatD [prt 0 n])
    ELitTrue -> prPrec i 10 (concatD [doc (showString "true")])
    ELitFalse -> prPrec i 10 (concatD [doc (showString "false")])
    EVar id -> prPrec i 10 (concatD [prt 0 id])
    EList exprs -> prPrec i 10 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    EListComp expr1 lval expr2 -> prPrec i 10 (concatD [doc (showString "["), prt 0 expr1, doc (showString "for"), prt 0 lval, doc (showString "in"), prt 0 expr2, doc (showString "]")])
    EDict dictmaps -> prPrec i 10 (concatD [doc (showString "#{"), prt 0 dictmaps, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print DictMap where
  prt i e = case e of
    EDictMap expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString ":"), prt 0 expr2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print RelOp where
  prt i e = case e of
    Lt -> prPrec i 0 (concatD [doc (showString "<")])
    Le -> prPrec i 0 (concatD [doc (showString "<=")])
    Gt -> prPrec i 0 (concatD [doc (showString ">")])
    Ge -> prPrec i 0 (concatD [doc (showString ">=")])
    Eq -> prPrec i 0 (concatD [doc (showString "==")])
    Ne -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print AddOp where
  prt i e = case e of
    Add -> prPrec i 0 (concatD [doc (showString "+")])
    Sub -> prPrec i 0 (concatD [doc (showString "-")])

instance Print MulOp where
  prt i e = case e of
    Mul -> prPrec i 0 (concatD [doc (showString "*")])
    Div -> prPrec i 0 (concatD [doc (showString "/")])
    Mod -> prPrec i 0 (concatD [doc (showString "%")])


