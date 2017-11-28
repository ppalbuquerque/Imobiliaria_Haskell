module Modulos.Arvore where
import Data.List
import Modulos.Helpers as Helpers
import Data.Typeable

verifica :: String -> IO()
verifica conta = do
  f <- readFile "/Users/luishenriquesilva/Desktop/hell_bank-master/DataBase/contas.txt"
  let fim = lines f
  --let q = words fim
  let seq = criar_lista fim
  let arv = fazABB seq
  let b = buscar conta arv
  if b == False then putStr $ error "Error!! Conta nao existe" else putStr "Conta aberta!!"
  print $ ""

criar_lista :: [String] -> [String]
criar_lista [] = []
criar_lista (x:xs) = busca_contas1 (words x) ++ criar_lista xs

busca_contas1 :: [String] -> [String]
busca_contas1 [] = []
busca_contas1 (x:xs) = if x == "numero:" then [head xs] else busca_contas1 xs

data Arvore t = Nulo | No (Arvore t) t (Arvore t)
                       deriving (Eq, Ord, Show)

fazABB :: (Ord t) => [t] -> Arvore t
fazABB [ ] = Nulo
fazABB (x:xs) = No (fazABB ys) x (fazABB zs) where (ys, zs) = particao (<=x) xs

particao :: (t -> Bool) -> [t] -> ([t], [t])
particao p xs = (filter p xs, filter (not.p) xs)


buscar :: (Ord t) => t -> Arvore t -> Bool
buscar elemento Nulo = False
buscar elemento (No arvEsq n arvDir)
  | (elemento < n) = buscar elemento arvEsq
  | (elemento == n) = True
  | (elemento > n) = buscar elemento arvDir
