module Modulos.Deposito where 
import Prelude
import System.IO
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.IO.Error
import Modulos.Arvore

transfere :: IO ()
depositando :: IO()
criar :: String -> IO()

depositando = do 
  arquivo <- readFile "DataBase/contas.txt"
  putStr "Buscar conta \n Número da conta: "
  num_conta <- getLine
  Modulos.Arvore.verifica num_conta
  putStr "Valor do Deposito: "
  deposito <- getLine

  let a = lines arquivo
  -- Valor depositado
  let des = read deposito :: Float
  -- Auxilio, divide a string em uma lista de substrings
  let corte = words (busca_conta num_conta (lines arquivo))
  -- Apresenta o saldo atual lido do arquivo
  let saldo_atual = read x :: Float where (x:xs) = split ')' (busca_saldo corte)
  -- Calcula o novo saldo, soma com o antigo
  let novo_saldo = depositos saldo_atual des
  -- Transforma pra String
  let meio = show novo_saldo
  let b = busca_saldo_escreve meio corte
  let final = concatena b
  let contas = busca_conta_escreve num_conta a final
  let escrevendo = saltar_linha contas 
  let escreve = unwords escrevendo
  return arquivo
  putStr ""
  criar escreve
  print $ "Valor de deposito : "
  print $ des
  print $ "Saldo Atual : " 
  print $ saldo_atual
  print $ "Novo Saldo : " 
  print $ novo_saldo

saldo = do
  arquivo <- readFile "DataBase/contas.txt"
  putStr "Buscar conta \n Número da conta: "
  num_conta <- getLine
  Modulos.Arvore.verifica num_conta
  let conta = words (busca_conta num_conta (lines arquivo))
  let saldo_atual = read x :: Float where (x:xs) = split ')' (busca_saldo conta)
  print $ saldo_atual


criar x = do 
  createDirectoryIfMissing True "DataBase"
  let arquivoContas = "DataBase/temp.txt"
  existeContas <- doesFileExist arquivoContas
  when (existeContas) (writeFile arquivoContas x)

juros = do 
  arquivo <- readFile "DataBase/contas.txt"
  putStr "Buscar conta \n Número da conta: "
  num_conta <- getLine
  Modulos.Arvore.verifica num_conta
  putStr "Porcentagem do juros: "
  porcentagem <- getLine

  let a = lines arquivo
  -- Valor depositado
  let des = read porcentagem :: Float
  -- Auxilio, divide a string em uma lista de substrings
  let corte = words (busca_conta num_conta (lines arquivo))
  -- Apresenta o saldo atual lido do arquivo
  let saldo_atual = read x :: Float where (x:xs) = split ')' (busca_saldo corte)
  -- Calcula o novo saldo, soma com o antigo
  let novo_saldo = calculoJuros saldo_atual des
  -- Transforma pra String
  let meio = show novo_saldo
  let b = busca_saldo_escreve meio corte
  let final = concatena b
  let contas = busca_conta_escreve num_conta a final
  let escrevendo = saltar_linha contas 
  let escreve = unwords escrevendo
  return arquivo
  putStr ""
  criar escreve
  print $ "Porcentagem do juros: "
  print $ des
  print $ "Saldo Atual : " 
  print $ saldo_atual
  print $ "Novo Saldo : " 
  print $ novo_saldo

transfere = do inpStr <- readFile "DataBase/temp.txt"
               writeFile "DataBase/contas.txt" inpStr

busca_saldo_escreve :: String -> [String] -> [String]
busca_saldo_escreve s1 [] = []
busca_saldo_escreve s1 (x:xs) = if x == "saldo:" then x:s1:(tail xs) else (x:(busca_saldo_escreve s1 xs))

concatena :: [String] -> String
concatena [] = ""
concatena (x:xs) = x ++ " " ++ concatena xs

busca_conta_escreve :: String -> [String] -> String -> [String]
busca_conta_escreve s1 [] s2 = []
busca_conta_escreve s1 (x:xs) s2 = if substring s1 x then ((s2):xs) else (x:(busca_conta_escreve s1 xs s2))

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

calculoJuros :: Float -> Float -> Float
calculoJuros a b = (1 + (b/100))*a

-- Funções de apoio-------------------------------------------------------------------------
saltar_linha :: [String] -> [String]
saltar_linha [] = []
saltar_linha (x:xs) = (x++"\n"):(saltar_linha xs)

split :: Char -> String -> [String]
split _ "" =  []
split c s  =  let (l, s') = break (== c) s
                 in  l : case s' of
                           [] -> []
                           (_:s'') -> split c s''

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
