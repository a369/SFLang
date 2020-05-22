module Lang.TypeChecker (typeCheck, linkedTypeCheck) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map               as M

import           Lang.Objects.Error
import           Lang.Objects.LinkState
import           Lang.Objects.Syntax

linkedTypeCheck :: LinkState Program TEnv
linkedTypeCheck = do
  m <- popA
  case m of
    Just (s, (d, p)) -> do
        lib <- foldM (\env d -> do
            m' <- findB d
            case m' of
                Just e' -> return (M.union e' env)
                Nothing -> errorLS (LinkError "missing dependency")
            ) envEmpty d
        let f = do
              res <- execIT lib (inferProgram p)
              return (M.difference res lib)
        insertB s d f
        linkedTypeCheck
    _                -> return ()

typeCheck :: Program -> Either GlobError Program
typeCheck = evalIT . inferProgram

-------------------------------------------------------------------------------
-- Datatypes and related help functions ---------------------------------------
-------------------------------------------------------------------------------

type TEnv = M.Map Id Type

envEmpty  :: TEnv
envEmpty  = M.empty

type IT a = StateT ([Id], TEnv) (Either GlobError) a

idInit :: [Id]
idInit = map ((++) "_" . show) [1..]

evalIT :: IT a -> Either GlobError a
evalIT it = evalStateT it (idInit, envEmpty)

execIT :: TEnv -> IT a -> Either GlobError TEnv
execIT imp it = do
    (_, e) <- execStateT it (idInit, imp)
    return e

stateLess :: IT a -> IT a
stateLess it = do
    s <- get
    r <- it
    put s
    return r

newVar :: String -> IT Type
newVar s = do
    (xs, e) <- get
    put ((tail xs), e)
    (return . TyVar) ( (head xs) ++ s)

newError :: GlobError -> IT a
newError = lift . Left

newErrorFS :: String -> IT a
newErrorFS = newError . TypeError

envLookup :: Id -> IT (Maybe Type)
envLookup i = do
    (_, e) <- get
    return (M.lookup i e)

envDelete :: Id -> IT ()
envDelete i = do
    (l, e) <- get
    put (l, M.delete i e)

envInsert :: Id -> Type -> IT ()
envInsert i t = do
    (l, e) <- get
    put (l, M.insert i t e)

envApplySubst :: Subst -> IT ()
envApplySubst s = do
    (l, e) <- get
    let e' = fmap (substitute s) e
    put (l, e')

envMove :: Id -> Id -> IT ()
envMove a b = do
    (l, e0) <- get
    let e1 = case M.lookup a e0 of
                Just v -> (M.insert b v . M.delete a) e0
                _      -> e0
    put (l, e1)

-------------------------------------------------------------------------------
-- Program Infer --------------------------------------------------------------
-------------------------------------------------------------------------------

inferProgram :: Program -> IT Program
inferProgram p = (foldr (>>) (return p) . map inferBind ) p

inferBind :: Bind -> IT ()
inferBind (Enum t  is) = (foldr (>>) (return ()) .
    map (\i -> envInsert i (TyRigid t))) is
inferBind (Bind i e t) = do
    let action = do
          checkBind t
          envInsert i t
          (t', s) <- foldExp inferExpAlgebra e
          return (t', s)
    (t', s) <- stateLess action
    u       <- unify t t'
    envInsert i (substitute u t)

checkBind :: Type -> IT ()
checkBind t = do
    (_, e) <- get
    let vs = M.elems e
    let vars = foldType ((++), (++), (++), [], [], return, const []) t
    _ <- foldM (\_ v -> do
        if elem (TyRigid v) vs
            then return ()
            else newErrorFS (v ++ " not declared ")
        ) () vars
    return ()

-------------------------------------------------------------------------------
-- Exp Infer ------------------------------------------------------------------
-------------------------------------------------------------------------------

type ITExp = IT (Type, Subst)

inferExpAlgebra :: ExpAlgebra (ITExp) (ITExp)
inferExpAlgebra = (
      inferFunc       -- Func
    , inferPrim       -- Prim
    , inferApp        -- App
    , inferProd       -- Prod
    , inferSumL       -- SumL
    , inferSumR       -- SumR
    , inferVar        -- Var
    , inferVal        -- Val
    , inferPatAlgebra -- Pattern
    )

inferFunc :: [(ITExp, ITExp)] -> ITExp
inferFunc (l:ls) = (stateLess . foldr foldFunc base) ls
    where
        base = inferCase l
        foldFunc b am = do
            (at, as) <- am
            envApplySubst as
            (bt, bs) <- inferCase b
            u <- unify (substitute bs at) bt
            return (substitute u bt, u `sappend` as `sappend` bs)

inferCase :: (ITExp, ITExp) -> ITExp
inferCase (p, b) = do
    let action = do
          (pt, ps) <- p
          bt'      <- newVar "body"
          (bt, bs) <- b
          let fsub = substitute (ps `sappend` bs)
          u <- unify (fsub bt') (fsub bt)
          return (TyArrow (substitute bs pt) bt, u)
    stateLess action

inferPrim :: Op -> ITExp
inferPrim o = return (typeOfOp o, sempty)

inferApp :: ITExp -> ITExp -> ITExp
inferApp a b = do
    let action = do
          (ta, sa) <- a
          envApplySubst sa
          (tb, sb) <- b
          ph <- newVar "appRes"
          u <- unify (substitute sb ta) (TyArrow tb ph)
          return (substitute u ph, u `sappend` sb `sappend` sa)
    stateLess action

inferProd :: ITExp -> ITExp -> ITExp
inferProd l r = do
    (tl, sl) <- l
    (tr, sr) <- r
    return (TyProd tl tr, sl `sappend` sr)

inferSumL :: ITExp -> ITExp
inferSumL l = do
    (tl, sl) <- l
    tr       <- newVar "R"
    return (TySum tl tr, sl)

inferSumR :: ITExp -> ITExp
inferSumR r = do
    tl       <- newVar "L"
    (tr, sr) <- r
    return (TySum tl tr, sr)

inferVar :: Id -> ITExp
inferVar i = do
    m <- envLookup i
    case m of
        Just t -> return (t, sempty)
        _      -> newErrorFS (i ++ " not declared")

inferVal :: Value -> ITExp
inferVal (VNum  _) = return (TyNum, sempty)
inferVal (VBool _) = return (TyBool, sempty)
inferVal (VLit  i) = do
    m <- envLookup i
    case m of
        Just t -> return (t, sempty)
        _      -> newErrorFS ("Couldn't find Type of Lit: " ++ i)

-------------------------------------------------------------------------------
-- Pat Infer ------------------------------------------------------------------
-------------------------------------------------------------------------------

inferPatAlgebra :: PatAlgebra (ITExp)
inferPatAlgebra = (
      inferVal    -- Val
    , inferPatVar -- Var
    , inferSumL   -- ChooseL
    , inferSumR   -- ChooseR
    , inferProd   -- Product
    )

inferPatVar :: Id -> ITExp
inferPatVar i = do
    t <- newVar i
    envInsert i t
    return (t, i =: t)


-------------------------------------------------------------------------------
-- Other Help funcitons -------------------------------------------------------
-------------------------------------------------------------------------------

unify :: Type -> Type -> IT Subst
unify (TyArrow a0 a1) (TyArrow b0 b1) = unify' a0 a1 b0 b1
unify (TyProd  a0 a1) (TyProd  b0 b1) = unify' a0 a1 b0 b1
unify (TySum   a0 a1) (TySum   b0 b1) = unify' a0 a1 b0 b1
unify  TyNum           TyNum          = return sempty
unify  TyBool          TyBool         = return sempty
unify (TyRigid a0)    (TyRigid b0   ) = 
    if a0 == b0 
        then return sempty
        else newErrorFS ("No unification possible between: "
                ++ show a0 ++ " and " ++ show b0)
unify (TyVar   a0   ) (TyVar   b0   ) =
    if (a0 == b0)
        then return sempty
        else return (a0 =: (TyVar b0))
unify (TyVar   a0   )  t              =
    if foldType ((||), (||), (||), False, False, const False, ((==) a0)) t
        then newErrorFS (a0 ++ "Ocrurs in " ++ show t)
        else return (a0 =: t)
unify  t              (TyVar   b0   ) = unify (TyVar b0) t
unify t0 t1 = newErrorFS ("No unification possible between: "
    ++ show t0 ++ " and " ++ show t1)

unify' :: Type -> Type -> Type -> Type -> IT Subst
unify' a0 a1 b0 b1 = do
    u0 <- unify a0 b0
    u1 <- unify a1 b1
    return (u0 `sappend` u1)

typeOfOp :: Op -> Type
typeOfOp Add  = TyNum  `TyArrow` (TyNum  `TyArrow` TyNum )
typeOfOp Sub  = TyNum  `TyArrow` (TyNum  `TyArrow` TyNum )
typeOfOp Mul  = TyNum  `TyArrow` (TyNum  `TyArrow` TyNum )
typeOfOp Quot = TyNum  `TyArrow` (TyNum  `TyArrow` TyNum )
typeOfOp Gt   = TyNum  `TyArrow` (TyNum  `TyArrow` TyBool)
typeOfOp Ge   = TyNum  `TyArrow` (TyNum  `TyArrow` TyBool)
typeOfOp Lt   = TyNum  `TyArrow` (TyNum  `TyArrow` TyBool)
typeOfOp Le   = TyNum  `TyArrow` (TyNum  `TyArrow` TyBool)
typeOfOp Eq   = TyNum  `TyArrow` (TyNum  `TyArrow` TyBool)
typeOfOp And  = TyBool `TyArrow` (TyBool `TyArrow` TyBool)
typeOfOp Or   = TyBool `TyArrow` (TyBool `TyArrow` TyBool)
typeOfOp Not  = TyBool `TyArrow`  TyBool

-------------------------------------------------------------------------------
-- Substitution ---------------------------------------------------------------
-------------------------------------------------------------------------------

-- Subst data type, monoid instance, and =: operator copied
-- from minHS concepts of program design

newtype Subst = Subst [(Id, Type)] deriving Show

sempty :: Subst
sempty = Subst []

sappend :: Subst -> Subst -> Subst
Subst a `sappend` Subst b = Subst $ map (fmap $ substitute $ Subst b) a
                                   ++ map (fmap $ substitute $ Subst a) b

(=:) :: Id -> Type -> Subst
a =: b = Subst [(a,b)]

subsAlg :: TypeAlgebra (Subst -> Type)
subsAlg = (
      (\l r s       -> TyArrow (l s) (r s))
    , (\l r s       -> TyProd  (l s) (r s))
    , (\l r s       -> TySum   (l s) (r s))
    , (\    s       -> TyNum              )
    , (\    s       -> TyBool             )
    , (\i   s       -> TyRigid  i         )
    , (\i (Subst s) -> case lookup i s of
                        Just t -> t
                        _      -> TyVar i )
    )

substitute :: Subst -> Type -> Type
substitute = flip $ foldType subsAlg
