import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Either

data Expr = Lit Integer | BinOp Op Expr Expr deriving Show
data Op = Plus | Mul deriving Show

def = emptyDef { reservedOpNames = ["*", "+"] }

TokenParser { parens = m_parens
            , reservedOp = m_reservedOp
            , natural = m_natural } = makeTokenParser def

expr1 :: Parser Expr
expr1 = buildExpressionParser table term
  where 
    table = [ [Infix (m_reservedOp "*" >> return (BinOp Mul)) AssocLeft
             ,Infix (m_reservedOp "+" >> return (BinOp Plus)) AssocLeft]]
    term = m_parens expr1 <|> (Lit <$> m_natural)

expr2 :: Parser Expr
expr2 = buildExpressionParser table term
  where 
    table = [ [Infix (m_reservedOp "+" >> return (BinOp Plus)) AssocLeft],
              [Infix (m_reservedOp "*" >> return (BinOp Mul)) AssocLeft]]
    term = m_parens expr2 <|> (Lit <$> m_natural)

eval :: Expr -> Integer
eval (Lit i) = i
eval (BinOp op e1 e2) = (mapOp op) (eval e1) (eval e2)
  where mapOp Plus = (+)
        mapOp Mul  = (*)

part0 :: Parser Expr -> [String] -> Integer
part0 parser = sum . map eval . rights . map (parse parser "")

part1 :: [String] -> Integer
part1 = part0 expr1

part2 :: [String] -> Integer
part2 = part0 expr2

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
