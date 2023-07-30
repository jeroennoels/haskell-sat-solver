module Global where

{-# INLINE optimized #-}
optimized :: Bool
optimized = False

newtype Lit = Lit Int deriving (Show, Eq)

newtype Clause = Clause [Lit] deriving Show

negation :: Lit -> Lit
negation (Lit i) = Lit (-i)

literals :: Clause -> [Lit]
literals (Clause xs) = xs
