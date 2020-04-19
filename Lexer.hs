module Lexer where
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.String ( Parser )


lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
            {
                T.commentStart = "{-"
              , T.commentEnd = "-}"
              , T.reservedOpNames = [ "+", "-", "*", "/","=",":=", ">", "<",">=","<=",
                                      "not", "and", "or" ]
              , T.reservedNames = [ "if", "then", "else", "while", "do","skip",
                                  "true", "false" ]

            }


identifier :: Parser String
identifier = T.identifier lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

integer :: Parser Integer
integer = T.integer lexer

semi :: Parser String
semi =  T.semi lexer

semiSep :: Parser a -> Parser [ a ]
semiSep = T.semiSep lexer