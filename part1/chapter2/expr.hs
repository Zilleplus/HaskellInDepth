{-# LANGUAGE OverloadedStrings #-}
module Main where

import TextShow(showbPrec, showb, TextShow, showbParen)

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)

myeval :: Num a => Expr a -> a
myeval (Lit n) = n
myeval (Add l r) = (myeval l) + (myeval r)
myeval (Mult l r) = (myeval l) * (myeval r)

instance TextShow a => TextShow (Expr a) where
    showbPrec p e = 
        case e of
          Lit a -> showb a
          Add e1 e2 -> showbHelper p 5 "+" e1 e2
          Mult e1 e2 -> showbHelper p 6 "*" e1 e2
      where
          showbHelper outerPrec thisPrec op e1 e2 =
              showbParen (outerPrec > thisPrec) 
              $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2
                      

main :: IO ()
main = do{
        putStr "Expr example \n"
         }
