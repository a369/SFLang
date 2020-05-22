module Main where

--
-- Small test program for the game language
-- Run from src otherwise the import tests don't work
--

import Lang.Compiler
import System.IO
import System.Directory
import Data.Maybe
import Control.Monad
import qualified Data.Text as T

folders :: [String]
folders = ["simpleTypes", "functions", "linker"]

main :: IO ()
main = do
    let path = "src/Test/Lang/testCases/"
    files <- foldM (\b a -> do
        gdc <- getDirectoryContents (path ++ a)
        let gdc' = map (\s -> path ++ a ++ "/" ++ s) gdc
        return (b ++ gdc') ) [] folders 
    (testFunc . catMaybes . map returnLuu) files
    where
        returnLuu s = case splitOn "." s of
                        [r, "luu"] -> Just r
                        otherwise  -> Nothing

testFunc :: [String] -> IO()
testFunc []     = do print "no tests"
testFunc (s:ss) = do
    compileWith (s ++ ".luu") (testFunc' (s:ss))

testFunc' :: [String] -> ControlFunc
testFunc' (s:ss) f = do
    outpH <- openFile (s ++ ".output") ReadMode
    outp  <- hGetContents outpH
    let (res, comp) = (show (f []), outp)
    case (res == comp, ss) of 
        (True , ns:_) -> do
            print (s ++ ": succes!") 
            compileWith (ns ++ ".luu") (testFunc' ss)
        (True , []   ) -> do
            print (s ++ ": succes!") 
            print "Tests Done"
        (False, _    ) -> do
            print (s ++ ": failure " ++ res ++ " != " ++ comp) 
    hClose outpH
testFunc' [] _ = do print "no tests"

splitOn :: String -> String -> [String]
splitOn s t = map T.unpack (T.splitOn (T.pack s) (T.pack t))
