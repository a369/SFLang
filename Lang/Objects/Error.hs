module Lang.Objects.Error where

-------------------------------------------------------------------------------
-- Error ----------------------------------------------------------------------
-------------------------------------------------------------------------------

data GlobError  = LinkError  String
                | TokenError String
                | LexError   String
                | TypeError  String
                | EvalError  String
    deriving (Show)

