 
--main = do
--  f <- readFile "DataBase/contas.txt"
--  x <- getLine f
--  print $ f

import Prelude

main = do
  -- Carrega o arquivo .txt com as contas existentes
  f <- readFile "DataBase/contas.txt"
  putStr "Buscar conta \n Número da conta: "
  num_conta <- getLine
  putStr "Valor do Deposito(Ponto flutuante): "
  deposito <- getLine
  -- Pego cada linha do BD que corresponde a uma conta e retorna uma lista de Strings
  --print $ confere(lines f)
  let a = lines f
  let des = read deposito :: Float
  --let conta = busca_conta "61618" (lines f)
  let corte = words (busca_conta num_conta (lines f))
  --let saldo = busca_saldo (words (busca_conta "61618" (lines f)))
  --let s = split ')' (busca_saldo (words (busca_conta "61618" (lines f))))
  let saldo_atual = read x :: Float where (x:xs) = split ')' (busca_saldo corte)
  let novo_saldo = depositos saldo_atual des
  let meio = show novo_saldo
  let b = busca_saldo_escreve meio corte
  let final = concatena b
  --print $ b
  --let inicio = concatena(take 7 (words (busca_conta "61618" (lines f))))
  --let fim = concatena (reverse (take 6 (reverse (words (busca_conta "61618" (lines f))))))
  --let final = inicio  ++ meio ++ " " ++ fim
  
  --print $ inicio
  print $ busca_conta_escreve num_conta a final

busca_saldo_escreve :: String -> [String] -> [String]
busca_saldo_escreve s1 [] = []
busca_saldo_escreve s1 (x:xs) = if x == "saldo:" then x:s1:(tail xs) else (x:(busca_saldo_escreve s1 xs))

concatena :: [String] -> String
concatena [] = ""
concatena (x:xs) = x ++ " " ++ concatena xs

-- Numero da conta - Lista de Contas - Conta final
busca_conta_escreve :: String -> [String] -> String -> [String]
busca_conta_escreve s1 [] s2 = []
busca_conta_escreve s1 (x:xs) s2 = if substring s1 x then (s2:xs) else (x:(busca_conta_escreve s1 xs s2))

busca_conta :: String -> [String] -> String
busca_conta s [] = "Conta nao encontrada"
busca_conta s (x:xs) = if substring s x then x else busca_conta s xs

busca_saldo :: [String] -> String
busca_saldo [] = "Vazio"
busca_saldo (x:xs) = if x == "saldo:" then y else busca_saldo xs where (y:ys) = xs

depositos :: Float -> Float -> Float 
depositos a b = if a < 0.0 then a else a+b

saques :: Float -> Float -> Float 
saques a b = if a < 0.0 then error "Não pode fazer saque" else a-b
 


-- Funções de apoio-------------------------------------------------------------------------

split :: Char -> String -> [String]
split _ "" =  []
split c s  =  let (l, s') = break (== c) s
                 in  l : case s' of
                           [] -> []
                           (_:s'') -> split c s''

confere :: [String] -> [String] 
confere [] = ["Erro: Nao possui nenhuma conta cadastrada!!"]
confere s = s

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
