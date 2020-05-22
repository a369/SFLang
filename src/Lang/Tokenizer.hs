module Lang.Tokenizer (
  tokenize,
  linkedTokenize)
where

import           Data.Functor.Identity
import           Text.Parsec

import           Lang.Objects.Error
import           Lang.Objects.LinkState
import           Lang.Objects.Tokens

type SParser a = ParsecT String () Identity a

linkedTokenize :: LinkState String [Token]
linkedTokenize = do
  m <- popA
  case m of
    Just (s, (d, a)) -> do
      insertB s d (tokenize a)
      linkedTokenize
    _                -> return ()

tokenize :: String -> Either GlobError [Token]
tokenize s = case (parse ((many . preWhiteSpace . choice) tokenParsers) "" s) of
    Left err -> (Left . TokenError . show) err
    Right t  -> Right (removeNoise t)

tokenParsers :: [SParser Token]
tokenParsers = [pID
              , pKey
              , pNum
              , pNegArr
              , pType
              , pLToken TR_    "("
              , pLToken T_R    ")"
              , pLToken TS_    "["
              , pLToken T_S    "]"
              , pLToken TC_    "{"
              , pLToken T_C    "}"
              , pLToken TBind  "="
              , pLToken TCol   ";"
              , pLToken TSCol  ";"
              , pLToken TCom   ","
              , pLToken TSum   "|"
              , pNL
              ]

pID :: SParser Token
pID = do
    i  <- lower
    is <- many alphaNum
    return (TID (i:is))

pKey :: SParser Token
pKey = do
    i  <- upper
    is <- many alphaNum
    case i:is of
        "True"  -> return $ TBool True
        "False" -> return $ TBool False
        e       -> return $ TKey e

pNum :: SParser Token
pNum = do
    i <- many1 digit
    return (TNum (read i))

pNL :: SParser Token
pNL = do
    _ <- char '\n'
    ws <- many (char ' ')
    case length ws of
        0 -> return TNL
        n -> return (TIndent n)

pNegArr :: SParser Token
pNegArr = do
    _ <- char '-'
    t <- (const TArrow <$> char '>')
        <|> (TNum . (-) 0 . read <$> many1 digit)
        <|> ((\_ i -> TNoise i) <$> char '-' <*> many (satisfy ((/=) '\n')))
    return t

pLToken :: Token -> String -> SParser Token
pLToken t s = do
    _ <- string s
    return t

pType :: SParser Token
pType = do
    _ <- char '!'
    t <- (const TTypeBind <$> char ':')
        <|> ((\i is -> TType (i:is)) <$> upper <*> many alphaNum)
        <|> ((\i is -> TTVar (i:is)) <$> lower <*> many alphaNum)
    return t

preWhiteSpace :: SParser a -> SParser a
preWhiteSpace p = do
    _  <- many (string " ")
    p
