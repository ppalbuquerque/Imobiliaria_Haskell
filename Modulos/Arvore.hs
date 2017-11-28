module Modulos.Arvore where
import Data.List
import Modulos.Helpers as Helpers

verifica_imovel_cod :: String -> String -> IO Bool
verifica_imovel_cod pCod tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let seq = criar_lista_imovel_cod fim
  let arv = fazABB seq
  let b = buscar pCod arv
  if b == False then putStr $ error "Error!! imovel não existe" else putStr ""
  return (b)

criar_lista_imovel_cod :: [String] -> [String]
criar_lista_imovel_cod [] = []
criar_lista_imovel_cod (x:xs) = busca_imovel_cod (words x) ++ criar_lista_imovel_cod xs

busca_imovel_cod :: [String] -> [String]
busca_imovel_cod [] = []
busca_imovel_cod (x:xs) = if x == "imovelid:" then [head xs] else busca_imovel_cod xs

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
