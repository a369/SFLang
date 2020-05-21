module Lang.Objects.LinkState where

import           Control.Monad.State

import           Lang.Objects.Error

-------------------------------------------------------------------------------
-- Link State -----------------------------------------------------------------
-------------------------------------------------------------------------------

type LinkElem a = (String, ([String], a))
type LinkEnv  a = [LinkElem a]

type LinkStateR a b c = StateT (LinkEnv a, LinkEnv b) (Either GlobError) c
type LinkState  a b   = LinkStateR a b ()

popA :: LinkStateR a b (Maybe (LinkElem a))
popA = do
    (a, b) <- get
    case null a of
        True  -> return Nothing
        False -> do
                    put (tail a, b)
                    (return . Just . head) a

putB :: LinkElem b -> LinkState a b
putB i = do
    (a, b) <- get
    put (a, b ++ [i])

findB :: String -> LinkStateR a b (Maybe b)
findB s = do
    (_, b) <- get
    case lookup s b of
        Just (_, el) -> return (Just el)
        _            -> return Nothing

insertB :: String -> [String] -> (Either GlobError b) -> LinkState a b
insertB _ _ (Left  e) = lift (Left e)
insertB s d (Right r) = do
    putB (s, (d, r))

runLS :: LinkState a b -> LinkEnv a -> (Either GlobError (LinkEnv b))
runLS m imp = do
    (_, (_, res)) <- runStateT m (imp, [])
    return res

errorLS :: GlobError -> LinkStateR a b c
errorLS = lift . Left
