module Lang.Objects.Tokens where

-------------------------------------------------------------------------------
-- Tokens ---------------------------------------------------------------------
-------------------------------------------------------------------------------

data Token  = TID    String -- Lowercase
            | TKey   String -- Uppercase
            | TNum   Int    -- Int
            | TBool  Bool   -- Bool
            | TType  String -- !Uppercase
            | TTVar  String -- !lowercase
            | TR_           -- (
            | T_R           -- )
            | TS_           -- [
            | T_S           -- ]
            | TC_           -- {
            | T_C           -- }
            | TArrow        -- ->
            | TBind         -- =
            | TTypeBind     -- !:
            | TSum          -- |
            | TCol          -- :
            | TSCol         -- ;
            | TCom          -- ,
            | TNoise String -- # commends
            | TNL           -- "\n"
            | TIndent Int   -- "\n "
            deriving (Show, Eq)

removeIndentation :: [Token] -> [Token]
removeIndentation ts = [t | t <- ts, f t]
    where
        f (TIndent _) = False
        f _           = True

removeNoise :: [Token] -> [Token]
removeNoise ts = [t | t <- ts, f t]
    where
        f (TNoise _) = False
        f _          = True
