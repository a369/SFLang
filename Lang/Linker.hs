module Lang.Linker (
  link,
  Link(..),
  Files)
where

import           Data.Functor.Identity
import           Data.List
import           Text.Parsec

import           Lang.Objects.Error
import           Lang.Objects.LinkState

type SParser a = ParsecT String () Identity a

data Link = Done (Either GlobError Files) | NotDone String (String -> Link)

type Files = LinkEnv String

link :: String -> Link
link = link' ([], []) "main"

link' :: ([String], Files) -> String -> String -> Link
link' (open, files) name new =
    case union open ( selNew files' dpds ) of
        []      -> Done (chain files')
        (f: fs) -> NotDone f (link' (fs, files') f)
    where
        dpds   = findDependencies new
        files' = (name, (dpds, new)) : files

chain :: Files -> Either GlobError Files
chain imp = do
    order <- chain' imp
    let ins s = case lookup s imp of
                  Just r -> (s, r)
                  _      -> (s, ([], ""))
    return $ map ins order

chain' :: Files -> Either GlobError [String]
chain' []  = return []
chain' imp = do
    (fen, _) <- r
    let nFen = ((/=) fen)
    let newImp = (filter (\(i, _) -> nFen i) .
                    map (\(n, (l, f)) -> (n, (filter nFen l, f))))
                        imp
    cr <- chain' newImp
    return (fen : cr)
    where
        r = case filter (\(_, (l, _)) -> null l) imp of
          []     -> Left (LinkError "no free dependencies")
          (r':_) -> Right r'

selNew :: Files -> [String] -> [String]
selNew _ []     = []
selNew m (f:fs) = case lookup f m of
    Just _ -> selNew m fs
    _      -> f : (selNew m fs)

findDependencies :: String -> [String]
findDependencies s = case parse (option [] (try findImports)) "" imp of
                        Left  err -> [show err]
                        Right dps -> dps
    where
        imp = filter ((/=) ' ') s

findImports :: SParser [String]
findImports = do
    _ <- string "--"
    _ <- string "#import"
    l <- many1 (alphaNum <|> char '/' )`sepBy` char ','
    let nl = map (\n -> n ++ ".luu") l
    return nl
