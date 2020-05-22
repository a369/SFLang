module Lang.Objects.Syntax where

-------------------------------------------------------------------------------
-- Syntax ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Id = String

type Lit = String

type Program    = [Bind]

data Bind       = Bind   Id Exp Type | Enum Id [Lit]

data Exp        = Func   [(Pattern, Exp)]
                | Prim   Op
                | App    Exp Exp
                | Prod   Exp Exp
                | SumL   Exp
                | SumR   Exp
                | Var    Id
                | Val    Value
                deriving (Show)

data Pattern    = Const     Value
                | Variable  Id
                | ChooseL   Pattern
                | ChooseR   Pattern
                | Both      Pattern Pattern

data Value      = VNum   Int
                | VBool  Bool
                | VLit   Lit
                deriving (Show)

data Op         = Add
                | Sub
                | Mul
                | Quot
                | Gt
                | Ge
                | Lt
                | Le
                | Eq
                | Not
                | And
                | Or
                deriving (Show)

data Type       = TyArrow Type Type
                | TyProd  Type Type
                | TySum   Type Type
                | TyNum
                | TyBool
                | TyRigid Id
                | TyVar   Id
                deriving (Eq)

-------------------------------------------------------------------------------
-- Folds and Algebras ---------------------------------------------------------
-------------------------------------------------------------------------------

type ExpAlgebra aExp aPattern = (
      [(aPattern, aExp)]       -> aExp
    , Op                       -> aExp
    , aExp -> aExp             -> aExp
    , aExp -> aExp             -> aExp
    , aExp                     -> aExp
    , aExp                     -> aExp
    , Id                       -> aExp
    , Value                    -> aExp
    , PatAlgebra aPattern
    )

foldExp :: ExpAlgebra aExp aPattern -> Exp -> aExp
foldExp (eFunc, eOp, eApp, eProd, eSumL, eSumR, eVar, eVal, pa) = fe
    where
        fe (Func l)     = eFunc $ map (\(p, e) -> (fp p, fe e)) l
        fe (Prim o)     = eOp o
        fe (App e0 e1)  = eApp  (fe e0) (fe e1)
        fe (Prod e0 e1) = eProd (fe e0) (fe e1)
        fe (SumL e)     = eSumL (fe e)
        fe (SumR e)     = eSumR (fe e)
        fe (Var i)      = eVar  i
        fe (Val v)      = eVal  v
        fp              = foldPattern pa


type PatAlgebra aPattern = (
      Value                 -> aPattern
    , Id                    -> aPattern
    , aPattern              -> aPattern
    , aPattern              -> aPattern
    , aPattern -> aPattern  -> aPattern
    )

foldPattern :: PatAlgebra aPattern -> Pattern -> aPattern
foldPattern (pCons, pVar, pL, pR, pB) = fp
    where
        fp (Const v)    = pCons v
        fp (Variable i) = pVar  i
        fp (ChooseL p)  = pL    (fp p)
        fp (ChooseR p)  = pR    (fp p)
        fp (Both p0 p1) = pB    (fp p0) (fp p1)


idPatternFold :: PatAlgebra Pattern
idPatternFold = (Const, Variable, ChooseL, ChooseR, Both)

instance Show Pattern where
    show (Const v)    = show v
    show (Variable i) = show i
    show (ChooseL p)  = "_LL: " ++ show p
    show (ChooseR p)  = "_RR: " ++ show p
    show (Both p0 p1) = "( " ++ show p0 ++ ", " ++ show p1 ++ " )"

type TypeAlgebra aType = (
      aType -> aType -> aType
    , aType -> aType -> aType
    , aType -> aType -> aType
    ,                   aType
    ,                   aType
    , Id             -> aType
    , Id             -> aType
    )

foldType :: TypeAlgebra aType -> Type -> aType
foldType (tArrow, tProd, tSum, tNum, tBool, tRig, tVar) = ft
    where
        ft (TyArrow a b) = tArrow (ft a) (ft b)
        ft (TyProd  a b) = tProd  (ft a) (ft b)
        ft (TySum   a b) = tSum   (ft a) (ft b)
        ft  TyNum        = tNum
        ft  TyBool       = tBool
        ft (TyRigid   i) = tRig i
        ft (TyVar     i) = tVar i

instance Show Type where
    show (TyArrow t0 t1) = "(" ++ show t0 ++ " -> " ++ show t1 ++ ")"
    show (TyProd  t0 t1) = "(" ++ show t0 ++ ", " ++ show t1 ++ ")"
    show (TySum   t0 t1) = show t0 ++ " | " ++ show t1
    show  TyNum          = "Num"
    show  TyBool         = "Bool"
    show (TyVar i)       = "v:"++ i
    show (TyRigid i)     = "r:"++ i

instance Show Bind where
    show (Bind i e t) = i ++ " !: " ++ show t ++ "\n" ++ i ++ " = " ++ show e
    show (Enum i l)   = i ++ " !: " ++ show l
