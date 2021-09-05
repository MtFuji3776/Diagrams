module GraphParser(
    expr
) where

import qualified Data.Attoparsec.Text as T
--import qualified Data.Attoparsec.Text.Internal as PT(Parser)
import Algebra.Graph
--import Control.Monad(mplus)
import Data.Text(Text)
import Data.Either(fromLeft,fromRight)
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types

type G = Graph Int

expr :: Parser Text G
expr = buildExpressionParser 
        [
             [Infix (do symb "*"; return Connect) AssocRight]
        ,    [Infix (do symb "+"; return Overlay) AssocRight]
        ]  term <?> "expression"

term = parens expr <|> vert <?> "simple expression"

vert = numb

parens p = do symb "("; e <- p; symb ")"; return e


-- 以下、attoparsec-exprより写経
data Assoc = AssocNone | AssocLeft | AssocRight

data Operator t a = Infix (Parser t (a -> a -> a)) Assoc
                  | Prefix (Parser t (a -> a))
                  | Postfix (Parser t (a -> a))

type OperatorTable t a = [[Operator t a]]

buildExpressionParser :: Monoid t => [[Operator t b]] -> Parser t b -> Parser t b
buildExpressionParser operators simpleExpr = foldl makeParser simpleExpr operators
        where
            makeParser term ops
                = let (rassoc,lassoc,nassoc,prefix,postfix) = foldr splitOp ([],[],[],[],[]) ops
                      rassocOp = choice rassoc
                      lassocOp = choice lassoc
                      nassocOp = choice nassoc
                      prefixOp = choice prefix
                      postfixOp= choice postfix
                      
                      ambigious assoc op = do
                            _ <- op
                            fail ("ambigious use of a " ++ assoc ++ " associative operator")
                      ambigiousRight = ambigious "right" rassocOp
                      ambigiousLeft  = ambigious "left" lassocOp
                      ambigiousNon   = ambigious "non" nassocOp

                      termP = do
                          pre <- prefixP
                          x <- term
                          post <- postfixP
                          return (post (pre x))
                      postfixP = postfixOp <|> return id
                      prefixP = prefixOp <|> return id
                      rassocP x = do
                          f <- rassocOp
                          y <- do 
                                z <- termP
                                rassocPl z
                          return (f x y)
                          <|> ambigiousLeft
                          <|> ambigiousNon
                      rassocPl x = rassocP x <|> return x
                      lassocP x = do
                          f <- lassocOp
                          y <- termP
                          lassocPl (f x y)
                        <|> ambigiousRight
                        <|> ambigiousNon
                      lassocPl x = lassocP x <|> return x
                      nassocP x = do
                            f <- nassocOp
                            y <- termP
                            ambigiousRight <|> ambigiousLeft <|> ambigiousNon <|> return (f x y)
                      in do
                          x <- termP
                          rassocP x <|> lassocP x <|> nassocP x <|> return x
            splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
                = case assoc of
                    AssocNone -> (rassoc,lassoc,op:nassoc,prefix,postfix)
                    AssocLeft -> (rassoc,op:lassoc,nassoc,prefix,postfix)    
                    AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)
            splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
                = (rassoc,lassoc,nassoc,op:prefix,postfix)
            splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
                = (rassoc,lassoc,nassoc,prefix,op:postfix)

token p = do
    a <- p
    return a

symb :: Text -> T.Parser Text
symb cs = token $ T.string cs

numb = Vertex <$> token (T.decimal)

