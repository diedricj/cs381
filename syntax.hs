module MiniLogo where

import Prelude hiding (Num)
import Data.List



-----Exercise 1------
--Part a
type Num = Int
type Name = String

data Pos = PN Num| PS Name
         deriving Show

data Mode = Up
          | Down
          deriving (Show, Eq)

data Pars = Par Name | ManyPars Name Pars
          deriving Show

data Vals = Val Num | ManyVals Num Vals
          deriving Show

data Cmd = Pen Mode
         | MoveTo (Pos, Pos)
         | Def Name Pars Cmd
         | Call Name Vals
         | MultiCmd Cmd Cmd
         deriving Show

--Part B

vector::Cmd
vector = Def "vector" (ManyPars "x1" (ManyPars "y1" (ManyPars "x2" (Par "y2"))))
         (MultiCmd (Pen Up)
         (MultiCmd (MoveTo (PS "x1", PS "y1"))
         (MultiCmd (Pen Down)
         (MultiCmd (MoveTo (PS "x2", PS "y2")) (Pen Up)))))
--Part C

steps :: Int -> Cmd
steps 0 = (Pen Up)
steps num = (MultiCmd (Pen Up) 
            (MultiCmd (MoveTo (PN num, PN num)) 
            (MultiCmd (Pen Down)
	    (MultiCmd (MoveTo (PN (num-1), PN num))
	    (MultiCmd (MoveTo (PN (num-1), PN (num-1))) (steps (num-1)))))))

----- Exercise 2-----
--Part A
data Circuit = Circuits Gates Links deriving Show
data Gates = Gate Num GateFn Gates | EmptyGate deriving Show
data GateFn = And | Or | Xor | Not deriving Show
data Links = From Num Num Num Num Links | EmptyLink deriving Show


--Part B
halfAdder :: Circuit
halfAdder = Circuits (Gate 1 Xor (Gate 2 And (EmptyGate))) (From 1 1 2 1 (From 1 2 2 2 (EmptyLink)))

--Part C
prettyGates :: Gates -> String
prettyGates EmptyGate = ""
prettyGates (Gate gn fn rest) = show gn ++ ":" ++ show fn ++ ";" ++ "\n" ++ prettyGates rest

prettyLinks :: Links -> String
prettyLinks EmptyLink = ""
prettyLinks (From a b c d rest) = "from " ++ show a ++ "." ++ show b ++ " to " ++ show c ++ "." ++ show d
				++ ";" ++ "\n" ++ prettyLinks rest

prettyCircuit :: Circuit -> String
prettyCircuit (Circuits gates links) = (prettyGates gates) ++ (prettyLinks links)



------Exercise 3----

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
	  deriving Show
	
data Op = Add | Multiply | Negate deriving Show

data Exp = Num Int
         | Apply Op [Exp]
	 deriving Show

--Part A
expr = Apply Multiply [(Apply Negate [(Apply Add [Num 3, Num 4])]), Num 7]

{- Part B
The second representation makes it easier to chain one command to multiple
integers, for instance if I wanted to add 1, 2, 3, and 4 together its as simple as Apply Add [1,2,3,4].
Whereas the first representation would require a lot more uses of Plus to achieve the same thing.  Because 
of this, it might be easier with the first expression to see the pairs of numbers which have operations applied
to them.
-}

--Part C
giveInt :: Expr -> Int
giveInt (N a) = a

translate :: Expr -> Exp
translate (N a) = Num a
translate (Plus a b) = Apply Add [translate a,translate b]
translate (Times a b) = Apply Multiply [translate a,translate b]
translate (Neg a) = Apply Negate [translate a]







