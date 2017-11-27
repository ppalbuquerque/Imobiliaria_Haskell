module Modulos.Helpers where

import Modulos.Banco as Banco
import Modulos.BancoDeDados as BancoDeDados
import Modulos.Imovel as Imovel

import System.Random
import System.Directory

getInt :: IO Int
getFloat :: IO Float
seletorTipos :: IO String
dadosImovel :: IO()
gerarid :: IO Int

getInt = readLn
getFloat = readLn

seletorTipos = do
  putStrLn "Tipo:"
  putStrLn "Escolha uma opção..."
  putStrLn "1 - Para aluguel"
  putStrLn "2 - Para venda"
  opcao <- readLn
  if opcao <= 0 || opcao > 2 then do
    putStrLn "Opção Inválida\n"
    seletorTipos
  else
    case opcao of
      1 -> return ("Aluguel")
      2 -> return ("Venda")

gerarid = do
   gen <- newStdGen
   let ns = randoms gen :: [Int]
   return ( head ns )


dadosImovel = do
  putStrLn "Endereço: "
  pEndereco <- getLine
  putStrLn "Preco: "
  pPreco <- getFloat
  putStrLn "Descricao: "
  pDesc <- getLine
  putStrLn "Area: "
  pArea <- getFloat
  putStrLn "Comodos: "
  pComodos <- getInt
  pTipos <- seletorTipos
  pId <- gerarid

  let dado = (Imovel {endereco=pEndereco, preco=pPreco, descricao=pDesc, area=pArea, comodos=pComodos, tipo = pTipos,imovelid = pId})
  BancoDeDados.salvar dado


saveListToFile :: String -> [String] -> IO()
saveListToFile _ [] = putStrLn "Banco de dados atualizados"
saveListToFile path (x : xs)  = do
   appendFile path (x ++ "\n")
   saveListToFile path xs

destroyFile :: String -> IO()
destroyFile path = removeFile path


printImoveis :: (String, String) -> String
printImoveis (codigo, endereco) = "Codigo: " ++ codigo ++ " Endereco: " ++ endereco