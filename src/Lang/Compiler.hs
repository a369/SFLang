{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric        #-}
module Lang.Compiler where

import           Control.Monad
import           Prelude                hiding (lex)
import           System.IO

import           Lang.Evaluator
import           Lang.Lexer
import           Lang.Linker
import           Lang.Tokenizer
import           Lang.TypeChecker

import           Lang.Objects.Syntax
import           Lang.Objects.Error
import           Lang.Objects.LinkState
import           Lang.Objects.Result

-- to remove
import           Lang.Helper.ProgramGeneric
-- tot hier

type ControlFunc = ((Program -> Either GlobError Result) -> IO ())

compileWith :: String -> ControlFunc -> IO ()
compileWith f func = do
    handle <- openFile f ReadMode
    s <- hGetContents handle
    linkFile func (link s)
    hClose handle

run :: String -> IO ()
run f = do
    handle <- openFile f ReadMode
    s <- hGetContents handle
    linkFile (\f -> print (f [])) (link s)
    hClose handle  

linkFile :: ControlFunc -> Link -> IO ()
linkFile func (Done (Right fs)) = do
    func (linkedCompile fs)
    return ()
linkFile func (NotDone f fs) = do
    print ("Open: " ++ f)
    handle <- openFile f ReadMode
    s <- hGetContents handle
    linkFile func (fs s)
    hClose handle
linkFile _ _ = do
    print "link error"

linkedCompile :: Files -> Program -> Either GlobError Result
linkedCompile f p = do
    tokens  <- runLS linkedTokenize f
    program <- runLS linkedLex tokens
    let program' = sideCompileProgram program p
    env     <- runLS linkedBuild program'
    tEnv    <- runLS linkedTypeCheck program'
    result  <- linkedEval env
    return result

sideCompileProgram :: LinkEnv Program -> Program -> LinkEnv Program
sideCompileProgram l p = map (\(n, (d, e)) -> if n == "main" 
                                                then (n, ("world":d, e))
                                                else (n, (d, e)))
                                                (("world", ([], p)) : l)

