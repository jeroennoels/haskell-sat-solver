module Global where

{-# INLINE optimized #-}
optimized :: Bool
optimized = False

type RandomInt = Int

newtype Var = Var Int deriving (Show, Eq, Ord)

newtype Lit = Lit Int deriving (Show, Eq)

newtype Clause = Clause [Lit] deriving (Show, Eq)

variable :: Lit -> Var
variable (Lit i) = Var (abs i)

negation :: Lit -> Lit
negation (Lit i) = Lit (-i)

literals :: Clause -> [Lit]
literals (Clause xs) = xs
