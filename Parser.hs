{-# LANGUAGE RankNTypes, LiberalTypeSynonyms,ImpredicativeTypes #-}
module Parser ( whileParser ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String ( Parser )
import Control.Applicative  hiding ( (<|>) )
import Lexer
import AST

aTable = [  [   Prefix ( Neg <$ reservedOp "-" )                ]

          , [   Infix  ( ABin Mul <$ reservedOp "*" ) AssocLeft
              , Infix  ( ABin Div <$ reservedOp "/" ) AssocLeft ]
          , [   Infix  ( ABin Add <$ reservedOp "+" ) AssocLeft
              , Infix  ( ABin Sub <$ reservedOp "-" ) AssocLeft ]
         ]



bTable = [  [  Prefix ( Not  <$ reservedOp "¬" )               ]

          , [  Infix  ( BBin And <$ reservedOp "∧" ) AssocLeft ]
          , [  Infix  ( BBin Or  <$ reservedOp "∨"  ) AssocLeft ]

         ]


aExpression :: Parser AExpr
aExpression = buildExpressionParser aTable aTerm where
         aTerm =  parens aExpression
              <|> Var <$> identifier
              <|> Num <$> integer




bExpression :: Parser BExpr
bExpression = buildExpressionParser bTable bTerm where
         bTerm =  parens bExpression
              <|> (  Con True   <$ reserved "true"  )
              <|> (  Con False  <$ reserved "false" )

              <|> try (  AL Eq <$>  ( aExpression  <*  reserved "=")
                                    <*>aExpression )
              <|> try (  AL GrEq <$>  ( aExpression  <*  reserved ">=")
                                                  <*>aExpression )
              <|> try (  AL LsEq <$>  ( aExpression  <*  reserved "<=")
                                                                <*>aExpression )
              <|> try (  AL Greater <$>  ( aExpression  <* reserved ">" )
                                    <*>    aExpression )
              <|> (  AL Less    <$>  ( aExpression  <* reserved "<" )
                                <*>    aExpression )




whileParser :: Parser Stmt
whileParser = whiteSpace *> stmtParser <* eof where
            stmtParser :: Parser Stmt
            stmtParser =  parens stmtParser
                      <|> List <$> sepBy stmtOne semi
            stmtOne :: Parser Stmt
            stmtOne =  parens stmtOne
              <|> ( Assing <$> ( Var <$> identifier )
                                 <*> ( reserved ":=" *> aExpression )  )
             <|> ( If <$> ( reserved "if" *> bExpression <* reserved "then" )
                            <*>   stmtParser
                            <*> ( reserved "else" *> stmtParser ) )
             <|> ( While <$> ( reserved "while" *> bExpression <* reserved "do" )
                            <*>   stmtParser )
                   <|> ( Skip <$ reserved "skip" )

