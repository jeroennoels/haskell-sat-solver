module Global where

{-# INLINE optimized #-}
optimized :: Bool
optimized = False

newtype Var = Var Int deriving (Show, Eq)

newtype Lit = Lit Int deriving (Show, Eq)

newtype Clause = Clause [Lit] deriving Show

variable :: Lit -> Var
variable (Lit i) = Var (abs i)

negation :: Lit -> Lit
negation (Lit i) = Lit (-i)

literals :: Clause -> [Lit]
literals (Clause xs) = xs
