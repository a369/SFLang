module Lang.Evaluator (
  evaluate,
  Env,
  linkedBuild,
  linkedEval)
where

import           Control.Monad.State.Lazy
import qualified Data.Map                 as M
import           Data.Maybe

import           Lang.Objects.Error
import           Lang.Objects.LinkState
import           Lang.Objects.Result
import           Lang.Objects.Syntax

linkedBuild :: LinkState Program Env
linkedBuild =  do
  m <- popA
  case m of
    Just (s, (d, p)) -> do
        lib <- foldM (\env d -> do
            m' <- findB d
            case m' of
                Just e' -> return (M.union e' env)
                Nothing -> errorLS (LinkError "missing dependency")
            ) envEmpty d
        let res = evalProgram lib p
        let env = M.difference res lib
        insertB s d (Right env)
        linkedBuild
    _                -> return ()

linkedEval :: LinkEnv Env -> Either GlobError Result
linkedEval l = case lookup "main" l of
                    Just (_, e) -> case envLookup "main" e of
                                    Just r -> Right r
                                    _      -> Left (EvalError "no main bind")
                    _          -> Left (EvalError "no main file")


evaluate :: [Env] -> Program -> Either GlobError Result
evaluate e p = case (envLookup "main" .
                     evalProgram (foldr M.union envEmpty e)) p of
    Just p -> Right p
    _      -> Left $ EvalError ""

envEmpty  = M.empty
envLookup = M.lookup
envInsert = M.insert
envUnion  = M.union
envDelete = M.delete
envMove i0 i1 e = case envLookup i0 e of
    Just v -> (envInsert i1 v . envDelete i0) e
    _      -> e

evalAlgebra :: ExpAlgebra ExpEval (Env -> Pattern)
evalAlgebra = (
      eFunc               -- Func
    , return . (RPrim []) -- Prim
    , eApp                -- App
    , (<*>) . (<$>) RProd -- Prod
    , (<$>) RLeft         -- SumL
    , (<$>) RRight        -- SumR
    , eVar                -- Var
    , eVal                -- Val
    , patternEval         -- Pattern
    )



eFunc :: [((Env -> Pattern), ExpEval)] -> ExpEval
eFunc l0 = do
    e0 <- get
    let m  = envLookup "__RecFun" e0
    let e1 = envDelete "__RecFun" e0
    put e1
    let i = case m of
                Just (RLit v) -> Just v
                otherwise     -> Nothing
    let l1 = map (applyEnv e1) l0
    return (RFunc i l1)
        where
            applyEnv e (p, ex) = (p e, ex, e)

eApp :: ExpEval -> ExpEval -> ExpEval
eApp e0 e1 = do
    x0 <- e0
    x1 <- e1
    case x0 of
        RFunc (Just i) l -> let (ee, env) = head (mapMaybe (eApp' x1 ) l) in
            return $ (evalState ee . envInsert i (RFunc (Just i) l)) env
        RFunc _ l -> let (ee, env) = head (mapMaybe (eApp' x1 ) l) in
            return $ (evalState ee env)
        RPrim l o -> return $ fromMaybe (RPrim (l ++ [x1]) o)
                                        (solfPrim o (l ++ [x1]))
        RDebug e  -> return $ RDebug e
        other     -> return $ RNum 0

eApp' :: Result -> (Pattern, ExpEval, Env) -> Maybe (ExpEval, Env)
eApp' r (p, expr, env0) = case matchPattern p r of
                Just env1 -> Just (expr, envUnion env1 env0)
                Nothing   -> Nothing

eVar :: Id -> ExpEval
eVar i = do
    s <- get
    case envLookup i s of
        Just v  -> return v
        Nothing -> return (RDebug s)

eVal :: Value -> ExpEval
eVal (VNum  n) = return (RNum  n)
eVal (VBool b) = return (RBool b)
eVal (VLit  l) = return (RLit  l)

solfPrim :: Op -> [Result] -> Maybe Result
solfPrim Add  [RNum  a, RNum  b] = Just $ RNum  (a +  b)
solfPrim Sub  [RNum  a, RNum  b] = Just $ RNum  (a -  b)
solfPrim Mul  [RNum  a, RNum  b] = Just $ RNum  (a *  b)
solfPrim Quot [RNum  a, RNum  b] = Just $ RNum  (a `div` b)
solfPrim Gt   [RNum  a, RNum  b] = Just $ RBool (a >  b)
solfPrim Ge   [RNum  a, RNum  b] = Just $ RBool (a >= b)
solfPrim Lt   [RNum  a, RNum  b] = Just $ RBool (a <  b)
solfPrim Le   [RNum  a, RNum  b] = Just $ RBool (a <= b)
solfPrim Eq   [RNum  a, RNum  b] = Just $ RBool (a == b)
solfPrim Not  [RBool a         ] = Just $ RBool (not  a)
solfPrim And  [RBool a, RBool b] = Just $ RBool (a && b)
solfPrim Or   [RBool a, RBool b] = Just $ RBool (a || b)
solfPrim _     _                 = Nothing

matchPattern :: Pattern -> Result -> Maybe Env
matchPattern (Const (VNum v0)) (RNum v1) = if v0 == v1
    then Just envEmpty
    else Nothing
matchPattern (Const (VBool v0)) (RBool v1) = if v0 == v1
    then Just envEmpty
    else Nothing
matchPattern (Const (VLit v0)) (RLit v1) = if v0 == v1
    then Just envEmpty
    else Nothing
matchPattern (Variable i) r = Just $ envInsert i r envEmpty
matchPattern (ChooseL p) (RLeft r)  = matchPattern p r
matchPattern (ChooseR p) (RRight r) = matchPattern p r
matchPattern (Both p0 p1) (RProd r0 r1) = do
    l <- matchPattern p0 r0
    r <- matchPattern p1 r1
    return $ envUnion r l
matchPattern _ _ = Nothing

eval :: Exp -> Result
eval e = evalState (foldExp evalAlgebra e) envEmpty

evalProgram :: Env -> Program -> Env
evalProgram = foldl f
    where
        f env (Bind i e t) = envInsert i
            (evalState
                (foldExp evalAlgebra e)
                (envInsert "__RecFun" (RLit i) env)
                ) env
        f env (Enum _  vs) = foldr (\v e -> envInsert v (RLit v) e) env vs

patternEval :: PatAlgebra (Env -> Pattern)
patternEval =(
      (\v e -> Const v)
    , (\i e -> case envLookup i e of
        Just (RLit v) -> Const (VLit v)
        _             -> Variable i)
    , (\p e -> ChooseL (p e))
    , (\p e -> ChooseR (p e))
    , (\p0 p1 e -> Both (p0 e) (p1 e)))
