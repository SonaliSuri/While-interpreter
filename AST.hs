{-# LANGUAGE GADTs #-}
module AST  where



data Opa = Add
         | Sub
         | Mul
         | Div
         deriving Show

data Opb = And 
         | Or 
         deriving Show

data Opr = Greater 
         | Less 
         | Eq
         | GrEq
         | LsEq
         deriving Show


data AExpr = Var String
           | Num Integer
           | Neg AExpr
           | ABin Opa AExpr AExpr
           deriving Show

data BExpr = Con Bool
           | Not BExpr
           | BBin Opb BExpr BExpr
           | AL Opr AExpr AExpr

           deriving Show




data Stmt = List [ Stmt ]
          | Assing  AExpr AExpr 
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving Show