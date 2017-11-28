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

listando :: String -> IO()
listando tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let imoveis = listar fim
  let ordena = ordenaTuplas imoveis
  putStrLn . unlines . map printImoveis $ imoveis


ordenaTuplas :: [(String, String)] -> [(String, String)]
ordenaTuplas [] = []
ordenaTuplas (h : t) = ordenaTuplas menores ++ [h] ++ ordenaTuplas maiores
  where menores = [ x | x <- t, x <= h ]
        maiores = [ x | x <- t, x > h ]


criar_lista :: [String] -> [String]
criar_lista [] = []
criar_lista (x:xs) = busca_contas1 (words x) ++ criar_lista xs

listar :: [String] -> [(String, String)]
listar [] = []
listar (x:xs) =  [(busca_id(words x),busca_endereco (words x))] ++ listar xs

busca_id :: [String] -> String
busca_id [] = ""
busca_id (x:xs) = if x == "imovelid:" then head xs else busca_id xs

busca_endereco :: [String] -> String
busca_endereco [] = ""
busca_endereco (x:xs) = if x == "endereco:" then busca_endereco_found xs else busca_endereco xs

busca_endereco_found :: [String] -> String
busca_endereco_found [] = ""
busca_endereco_found (x:xs) = if x == "," then "" else x ++ " " ++ busca_endereco_found xs

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
