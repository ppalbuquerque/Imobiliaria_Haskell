module Modulos.BancoDeDados where

import System.Directory
import Control.Monad
import Modulos.Imovel as Imovel
import Modulos.Pessoa as Pessoa
import Modulos.Aluguel as Aluguel
import Modulos.Utils as Utils

-- Cria o diretório DataBase e os arquivos
criar :: IO()
-- Gera formato para ser salvo no arquivo de imoveis_disponiveis.txt
geraFormato_imovel :: Imovel -> String
-- Gera formato para ser salvo no arquivo de pessoas.txt
geraFormato_pessoa :: Pessoa -> String
-- Salva imoveis utilizando appendFile no arquivo de imoveis_disponiveis.txt
salvar_imovel :: Imovel -> IO()
-- Salva pessoas utilizando appendFile no arquivo de pessoas.txt
salvar_pessoa :: Pessoa -> IO()
-- Busca uma pessoa no banco de dados

criar = do
    createDirectoryIfMissing True "DataBase"
    let arquivoImoveis = "DataBase/imoveis.txt"

    existeImoveis <- doesFileExist arquivoImoveis
    when (not existeImoveis) (writeFile arquivoImoveis "")


salvar_imovel p = do
  appendFile "DataBase/imoveis.txt" (geraFormato_imovel p ++ "\n")
  putStr "Banco de dados atualizado!\n"

salvar_pessoa p = do
  let arquivo = "DataBase/Pessoas/pessoas.txt"
  empty <- isFileEmpty arquivo
  if empty then appendFile arquivo (geraFormato_pessoa p) else appendFile arquivo ("\n" ++ geraFormato_pessoa p)
  putStr "Banco de dados atualizado!\n"

salvar_aluguel p = do
  let arquivo = "DataBase/Alugueis/alugueis.txt"
  empty <- isFileEmpty arquivo
  if empty then appendFile arquivo (gerarFormato_aluguel p) else appendFile arquivo ("\n" ++ gerarFormato_aluguel p)
  putStr "Banco de dados atualizado!\n"

geraFormato_imovel p = "{Imovel( endereco: " ++ endereco p ++ " , preco: " ++ show(preco p) ++ " , descricao: " ++ show(area p) ++ " , comodos: " ++ show(comodos p) ++ " , tipo: " ++ tipo p ++ " , imovelid: " ++ show(imovelid p) ++ " )}"
geraFormato_pessoa p = "{Pessoa( nomePessoa: " ++ nomePessoa p ++ " , cpf: " ++ cpf p ++ " , idade: " ++ show(idade p) ++ " )}"
gerarFormato_aluguel p = "{Alugue( imovel: " ++ show((imovelid (imovel p))) ++ " , locatario: " ++ (cpf (locatario p)) ++ " , dataV: " ++ dataV p ++ " )}"

isFileEmpty :: String -> IO Bool
isFileEmpty path = do
  f <- readFile path
  let fim = lines f
  print $ fim
  let line = Utils.extract_string fim
  if line == "Vazio" then return True else return False
