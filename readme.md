## The project


## Components of the compiler
__The Linker__ is capable of importing multiple files from the main file, importing files from imported files, importing one file from multiple files, and culling the imported environment so you can only access binds from imported files (and not from files imported by imported files) . The Linker resolves all dependencies and if there are no import cycles and there is a main file it returns an evaluation order. This order is propagated with the LinkState Monad  through the entire compilation stack. 

__The Tokenizer__ is built with the parsec library, it is very basic and simplified by having most tokens start with a unique symbol (much like the $-symbol in PHP).

__The Lexer__ builds a program data structure from the tokens again with the parsec library. It's feature set is minimal for example it does not support infix operators.

__The Type-Checker__ is based on lessons learned from Concepts of Program Design but improved with a StateT Monad transformer. We first tried making it purely bottom up without substitution or unification but this did not work out because of recursion. 

__The Evaluator__ uses a State Monad with an environment to string global and local scope through the evaluation process, so a very straightforward evaluator.
The entire stack is built with an "Either GlobalError a" monad to handle errors in any part of the compilation process. 

## Syntax 


A program in LambdaUU (luu) consists of a list of Binds separated by newlines. The file you give the compiler should contain a main Bind, any imported files don't need a main bind. You can only call variables that have been bound before use.

Indentation (a newline followed by whitespace) will be seen as continuation of the previous line.

    "A"   := The string A
    
    a | b := Either a or b
    
    [a]*  := 0 or more a
    
    [a]+  := 1 or more a
    
    CID   := An uppercase letter followed by 0 or more alpha num.
    
    LID   := A lowercase letter followed by 0 or more alpha num.
    
    Bool  := "True" | "False"
    
    Int   := "0" | "1" | "-1" | "2" | "-2" | ...
    
    Bind  := EnumDeclaration | GlobalConstant
    
    EnumDeclaration :=
    "\n""!"CID "=" LID [ "|" LID ]*
    
    GlobalConstant :=
    "\n"LID "!:" Type
    "\n"LID "=" Exp
    
    Type := "!"CID                            -- Rigid type (Int, Bool or enum)
                | "!"LID                      -- Type variable
                | "(" Type ")"  
                | Type "->" Type  
                | "(" Type "," Type ")"  
                | Type "|" Type
    
    Exp :=      CID                            -- Built in functions
              | LID                            -- Variables and enum values
              | Bool
              | Int
              | Exp Exp                        -- Apply  
              | Lambda
              | "(" Exp "," Exp ")"
    
    Lambda := "L" "(" Pattern "->" Exp ")"
            | [ "L" "C" "(" Pattern "->" Exp ")" ]+
    
    Pattern := LID
                      | Bool
                      | Int
                      | "LL" Pattern
                      | "RR" Pattern
                      | "(" Pattern "," Pattern ")"
                      | "(" Pattern ")"
    
    Comments :=
    "--" anything "\n"
    
    Import :=
    "--" "#import" FilePath [ "," FilePath ]*
    Note: 
    This should be the first line of a program (so no newlines before the import statement), 
    second the file path should be relative from where the compiler is running 
    and should not contain a ".luu" extension (the file itself sould).
    
    
    Build in functions (not infix)
    +     := Add  !: !Int  -> !Int  -> !Int
    -     := Sub  !: !Int  -> !Int  -> !Int
    *     := Mul  !: !Int  -> !Int  -> !Int
    /     := Quot !: !Int  -> !Int  -> !Int
    >     := Gt   !: !Int  -> !Int  -> !Bool
    >=    := Ge   !: !Int  -> !Int  -> !Bool
    <     := Lt   !: !Int  -> !Int  -> !Bool
    <=    := Le   !: !Int  -> !Int  -> !Bool
    ==    := Eq   !: !Int  -> !Int  -> !Bool
    !     := Not  !: !Bool -> !Bool
    &&    := And  !: !Bool -> !Bool -> !Bool
    ||    := Or   !: !Bool -> !Bool -> !Bool
    Left  := LL   !: !a    -> (!a | !b)
    Right := RR   !: !b    -> (!a | !b)
    
    Examples:
    -- #import code/something, code/somethingElse
    
    if !: !Bool -> !a -> !a -> !a
    if = L C (True   -> L (a -> L (b -> a)))
           C (False  -> L (a -> L (b -> b)))
    
    pred !: !Int -> !Int
    pred = L (a -> Sub a 1)
    
    pow !: !Int -> !Int -> !Int
    pow = L (a -> L C (0 -> 1)
                    C (n -> Mul a (pow a (pred n))))
    
    fac !: !Int -> !Int
    fac = L C (0 -> 1)
            C (n -> (Mul n (fac (pred n))))
    
