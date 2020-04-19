module Interpreter ( evalProgram )  where

import qualified Data.Map as M
import AST

type Store = M.Map String Integer

evalA :: AExpr -> Store -> Integer
evalA ( Var v ) s  =  M.findWithDefault 0 v s
evalA ( Num n ) _ = n
evalA ( Neg e ) s = - ( evalA e  s ) 
evalA ( ABin Add e1 e2 ) s = evalA e1 s + evalA e2 s
evalA ( ABin Sub e1 e2 ) s = evalA e1 s - evalA e2 s
evalA ( ABin Mul e1 e2 ) s = evalA e1 s * evalA e2 s
evalA ( ABin Div e1 e2 ) s = div ( evalA e1 s ) ( evalA e2 s )



evalB :: BExpr -> Store -> Bool
evalB ( Con b ) _ = b
evalB ( Not e ) s = not ( evalB e s )

evalB ( BBin And e1 e2 ) s = ( && ) ( evalB e1 s )  ( evalB e2 s )
evalB ( BBin Or  e1 e2 ) s = ( || ) ( evalB e1 s )  ( evalB e2 s )

evalB ( AL Greater e1 e2 ) s = ( evalA e1 s ) > (  evalA e2 s )
evalB ( AL Less e1 e2 ) s = (evalA e1 s)<(evalA e2 s )
evalB (AL Eq e1 e2 ) s = (evalA e1 s) == (evalA e2 s)
evalB (AL GrEq e1 e2 ) s = (evalA e1 s) >= (evalA e2 s)
evalB (AL LsEq e1 e2 ) s = (evalA e1 s) <= (evalA e2 s)

interpret :: Stmt -> Store -> Store
interpret ( Skip ) s = s
interpret ( Assing ( Var v ) expr ) s =  M.insert v ( evalA expr s )  s
interpret ( List [] ) s = s
interpret ( List ( x : xs ) ) s = interpret ( List xs ) ( interpret x s )
interpret ( If e st1 st2 ) s
          | evalB e s = interpret st1 s
          | otherwise = interpret st2 s

interpret ( While e st ) s 
    | not t = s   
    | otherwise =  interpret ( While e st ) w 
    where
     t = evalB e s
     w = interpret st s

evalProgram :: Stmt -> Store
evalProgram st = interpret st M.empty

