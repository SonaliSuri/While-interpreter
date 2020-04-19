{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Text.Parsec
import Parser
import Interpreter
import Control.Monad (mfilter)
import Data.Text(pack, unpack, replace)
import Data.Text
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map
import Data.List
import Data.Text

import Control.Monad

getLines = do
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      --writeFile "myfile" xs
      return (x:xs)

main = do
    --program   <- readFile "Gcd.while"
    program <- getContents
    --jk <- getLines
    --print jk
    --let replace a b = map $ maybe b id . mfilter (/= a) . Just

    --let t = replace 'x' 'X' program

    --print t


    let m= T.pack program
    --print m
    let t = replace ">" "> " m
    --print t
    let r = replace "<" "< " t

    let mj =replace "< =" "<= " r

    let tr = replace "> =" ">=" mj

    let p = replace "=" "= " tr

    let u = replace "{" " " p

    let o = replace "}" " " u

    --print o


    --print a
    let q = T.unpack o

    --print q


    case parse whileParser "" q of
        Left e -> print e >> fail "Parse Error"
        Right ast -> do
                      let x = evalProgram  ast
                      --print x
                      let res =  M.toList x
                      let ress = [a ++ " → " ++ (show b) | (a, b) <- res]
                      let resss = Data.List.intercalate ", " ress
                      let fin = "{" ++ resss ++ "}"
                      putStrLn fin
















