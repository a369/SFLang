module Lang.Objects.Result where

import           Control.Monad.State.Lazy
import qualified Data.Map                 as M

import           Lang.Objects.Syntax

-------------------------------------------------------------------------------
-- Link State -----------------------------------------------------------------
-------------------------------------------------------------------------------

type Env = M.Map Id Result

type ExpEval = State Env Result

data Result = RNum   Int
            | RBool  Bool
            | RLit   Lit
            | RLeft  Result
            | RRight Result
            | RProd  Result Result
            | RFunc  (Maybe Id) [(Pattern, ExpEval, Env)]
            | RPrim  [Result] Op
            | RDebug Env

instance Show Result where
    show (RNum i)    = show i
    show (RBool i)   = show i
    show (RLit i)    = i
    show (RLeft i)   = show i
    show (RRight i)  = show i
    show (RProd a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
    show (RDebug e)  = "Debug " ++ show e
    show _           = "Other"
