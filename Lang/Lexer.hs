{-# LANGUAGE FlexibleContexts #-}
module Lang.Lexer (
  lex,
  linkedLex)
where

import           Data.Functor.Identity
import           Prelude                hiding (lex)
import           Text.Parsec

import           Lang.Objects.Error
import           Lang.Objects.LinkState
import           Lang.Objects.Syntax
import           Lang.Objects.Tokens

type TParser a = ParsecT [Token] () Identity a

linkedLex :: LinkState [Token] Program
linkedLex = do
  m <- popA
  case m of
    Just (s, (d, a)) -> do
      insertB s d (lex a)
      linkedLex
    _                -> return ()

lex :: [Token] -> Either GlobError Program
lex t = case (parse (pProgram) "" (removeIndentation t)) of
                Left err ->
                    (Left . LexError . show ) err
                Right r  -> Right r

pProgram :: TParser Program
pProgram = flip const <$> many (sThis () TNL) <*>
        (many1 (const <$> (pBind <|> pTBind) <*> many (sThis () TNL)))

pBind :: TParser Bind
pBind = do
    i <- sID (const True)
    t <- ((\_ t' -> t')        <$> sThis () TTypeBind <*> pTarr)
    _ <- many1 (sThis () TNL)
    _ <- sID ((==) i)
    b <- ((\_ e -> Bind i e t) <$> sThis () (TBind)   <*> pExpr)
    return b

pTBind :: TParser Bind
pTBind = do
    i <- sType (const True)
    _ <- sThis () TBind
    b <- (\l ls -> Enum i (l:ls)) <$> sID (const True)
            <*> many ((\_ l' -> l') <$> sThis () TSum <*> sID (const True))
    return b

pTarr :: TParser Type
pTarr = chainr1 pTterm (sThis TyArrow TArrow)

pTterm :: TParser Type
pTterm = do
    t <- ((brackedR) ((\t f -> f t) <$> pTarr <*>
            (((sThis (\r l -> TyProd l r) TCom) <*> pTarr)
        <|> option id ((sThis (\r l -> TySum  l r) TSum) <*> pTarr))))
            <|> (const TyNum  <$> sType ((==) "Int" ))
            <|> (const TyBool <$> sType ((==) "Bool"))
            <|> (TyRigid      <$> sType (const True))
            <|> (TyVar        <$> sTVar (const True))
    return t


pExpr :: TParser Exp
pExpr = foldl1 App <$> many1 pTerm

pTerm :: TParser Exp
pTerm = do
    t <- ((try . brackedR) ((\l _ r -> Prod l r) <$> pExpr <*> sThis () TCom
        <*> pExpr))
        <|> (brackedR pExpr
            <|> parseOp
            <|> (Val <$> parseValue)
            <|> (Var <$> sID (const True))
            <|> ((\_ e -> SumL e) <$> sThis () (TKey "LL") <*> pExpr)
            <|> ((\_ e -> SumR e) <$> sThis () (TKey "RR") <*> pExpr))
            <|> parseLambda
    return t

parseLambda :: TParser Exp
parseLambda = (\_ cs -> Func cs) <$> sThis () (TKey "L") <*>(
                            (return <$> parseCase) <|>
                            (many1( flip const <$> sThis () (TKey "C")
                            <*> parseCase)))

parseCase :: TParser (Pattern, Exp)
parseCase = brackedR ( (\p _ e -> (p , e)) <$> parsePattern <*> sThis () TArrow
                                <*> pExpr)

ops :: [(String, Op)]
ops = [("Add", Add), ("Sub", Sub), ("Mul", Mul), ("Quot", Quot), ("Gt", Gt),
        ("Ge", Ge), ("Lt", Lt), ("Le", Le), ("Eq", Eq), ("Not", Not),
        ("And", And), ("Or", Or)]

parseOp :: TParser Exp
parseOp = do
    k <- sKey (\s -> head([True |o <- ops, fst o == s] ++ [False]))
    return (Prim (head [snd o | o <- ops, fst o == k]))

parsePattern :: TParser Pattern
parsePattern = do
    v <-    (((try . brackedR) ((\l _ r -> Both l r) <$> parsePattern
        <*> sThis () TCom <*> parsePattern))
        <|>  canBrackedR (
                (Const <$> parseValue)
            <|> (Variable <$> sID (const True))
            <|> ((\_ p -> ChooseL p) <$> sThis () (TKey "LL") <*> parsePattern)
            <|> ((\_ p -> ChooseR p) <$> sThis () (TKey "RR") <*> parsePattern)
            ))
    return v

parseValue :: TParser Value
parseValue = do
    v <-    (VNum  <$> sNum  (const True))
        <|> (VBool <$> sBool (const True))
    return v

-------------------------------------------------------------------------------
-- Helper functions -----------------------------------------------------------
-------------------------------------------------------------------------------

sID :: (String -> Bool) -> TParser String
sID f = do
    t <- tSatisfy f'
    case t of
        (TID s) -> return s
        _       -> undefined
        where
            f' (TID s) = f s
            f' _       = False

sKey :: (String -> Bool) -> TParser String
sKey f = do
    t <- tSatisfy f'
    case t of
        (TKey s) -> return s
        _        -> undefined
        where
            f' (TKey s) = f s
            f' _        = False

sType :: (String -> Bool) -> TParser String
sType f = do
    t <- tSatisfy f'
    case t of
        (TType s) -> return s
        _         -> undefined
        where
            f' (TType s) = f s
            f' _         = False

sTVar :: (String -> Bool) -> TParser String
sTVar f = do
    t <- tSatisfy f'
    case t of
        (TTVar s) -> return s
        _         -> undefined
        where
            f' (TTVar s) = f s
            f' _         = False

sNum :: (Int -> Bool) -> TParser Int
sNum f = do
    t <- tSatisfy f'
    case t of
        (TNum i) -> return i
        _        -> undefined
        where
            f' (TNum i) = f i
            f' _        = False

sBool :: (Bool -> Bool) -> TParser Bool
sBool f = do
    t <- tSatisfy f'
    case t of
        (TBool s) -> return s
        _         -> undefined
        where
            f' (TBool s) = f s
            f' _         = False

sThis :: a -> Token -> TParser a
sThis a m = do
    _ <- tSatisfy ((==) m)
    return a

brackedR :: TParser a -> TParser a
brackedR = between (sThis () TR_) (sThis () T_R)

canBrackedR :: TParser a -> TParser a
canBrackedR p = brackedR p <|> p

tSatisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
tSatisfy f = tokenPrim  show
                        (\p _ _ -> incSourceLine p 1)
                        (\t -> if f t then Just t else Nothing)
