module Modulos.Helpers where

import Modulos.BancoDeDados as BancoDeDados
import Modulos.Imovel as Imovel

import System.Random
import System.Directory
import Control.Monad

getInt :: IO Int
getFloat :: IO Float
seletorTipos :: IO String
dadosImovel :: IO()
gerarid :: IO Int

getInt = readLn
getFloat = readLn

criar_arquivos = do
    createDirectoryIfMissing True "DataBase"
    createDirectoryIfMissing True "DataBase/Vendas/"
    createDirectoryIfMissing True "DataBase/Relatorios/"
    createDirectoryIfMissing True "DataBase/Alugueis/"
    createDirectoryIfMissing True "DataBase/Pessoas/"
    let arquivoImoveisVendasV = "DataBase/Vendas/imoveis_vendidos.txt"
    let arquivoImoveisVendasD = "DataBase/Vendas/imoveis_disponiveis.txt"
    let arquivoImoveisAlugueisV = "DataBase/Alugueis/imoveis_vendidos.txt"
    let arquivoImoveisAlugueisD = "DataBase/Alugueis/imoveis_disponiveis.txt"
    let arquivoImoveisAlugueisA = "DataBase/Alugueis/alugueis.txt"
    let arquivoPessoas = "DataBase/Pessoas/pessoas.txt"

    existe <- doesFileExist arquivoImoveisVendasV
    when (not existe) (writeFile arquivoImoveisVendasV "")
    existe <- doesFileExist arquivoImoveisVendasD
    when (not existe) (writeFile arquivoImoveisVendasD "")
    existe <- doesFileExist arquivoImoveisAlugueisD
    when (not existe) (writeFile arquivoImoveisAlugueisD "")
    existe <- doesFileExist arquivoImoveisAlugueisA
    when (not existe) (writeFile arquivoImoveisAlugueisA "")
    existe <- doesFileExist arquivoImoveisAlugueisV
    when (not existe) (writeFile arquivoImoveisAlugueisV "")
    existe <- doesFileExist arquivoPessoas
    when (not existe) (writeFile arquivoPessoas "")

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
      1 -> return ("Alugueis")
      2 -> return ("Vendas")

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
  BancoDeDados.salvar_imovel pTipos dado


saveListToFile :: String -> [String] -> IO()
saveListToFile _ [] = putStrLn "Banco de dados atualizados"
saveListToFile path (x : xs)  = do
   empty <- isFileEmpty path
   if empty then appendFile path (x) else appendFile path ("\n" ++ x)
   saveListToFile path xs

destroyFile :: String -> IO()
destroyFile path = do
   removeFile path
   writeFile path ""

printImoveis :: (String, String) -> String
printImoveis (codigo, endereco) = "Codigo: " ++ codigo ++ " Endereco: " ++ endereco

printImoveis_detalhado_1 :: (String, String, String, String, String) -> String
printImoveis_detalhado_1 (endereco, preco, descricao, comodos, codigo ) = "Endereço: " ++ endereco ++ " Preço: " ++ preco ++ " Descrição: " ++ descricao ++ " Comodos: " ++ comodos ++ " Código: " ++ codigo

printImoveis_detalhado_2 :: (String, String, String, String, String) -> String
printImoveis_detalhado_2 (preco, endereco, descricao, comodos, codigo ) = "Preço: " ++ preco ++ " Endereço: " ++ endereco ++ " Descrição: " ++ descricao ++ " Comodos: " ++ comodos ++ " Código: " ++ codigo

printImoveis_detalhado_3 :: (String, String, String, String, String) -> String
printImoveis_detalhado_3 (codigo, preco, descricao, comodos, endereco ) = "Código: " ++ codigo ++ " Preço: " ++ preco ++ " Descrição: " ++ descricao ++ " Comodos: " ++ comodos ++ " Endereço: " ++ endereco

printPessoas :: (String, String, String) -> String
printPessoas (cpf, nome, idade) = "CPF: " ++ cpf ++ " Nome: " ++ nome ++ " Idade: " ++ idade

printPessoas_2 :: (String, String, String) -> String
printPessoas_2 (nome, cpf, idade) = "Nome: " ++ nome ++ " CPF: " ++ cpf ++ " Idade: " ++ idade

printPessoas_3 :: (String, String, String) -> String
printPessoas_3 (idade, nome, cpf) = "Idade: " ++ idade ++ " Nome: " ++ nome ++ " CPF: " ++ cpf

printAluguel_1 :: (String, String, String) -> String
printAluguel_1 (imovel_cod, pessoa_cod, dataV) = "Imovel: " ++ imovel_cod ++ "Locatário: " ++ pessoa_cod ++ "Data De Pagamento: " ++ dataV

printAluguel_2 :: (String, String, String) -> String
printAluguel_2 (pessoa_cod, imovel_cod, dataV) = "Locatário: " ++ pessoa_cod ++ "Imovel: " ++ imovel_cod ++ "Data De Pagamento: " ++ dataV
