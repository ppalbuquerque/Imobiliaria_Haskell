module Modulos.BancoDeDados where

import System.Directory
import Control.Monad
import Modulos.Imovel as Imovel
import Modulos.Pessoa as Pessoa

-- Cria o diretório DataBase e os arquivos contas.txt
-- e transferencias.txt se não existir.
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
  appendFile "DataBase/Pessoas/pessoas.txt" (geraFormato_pessoa p ++ "\n")
  putStr "Banco de dados atualizado!\n"

geraFormato_imovel p = "{Imovel( endereco: " ++ endereco p ++ " , preco: " ++ show(preco p) ++ " , descricao: " ++ show(area p) ++ " , comodos: " ++ show(comodos p) ++ " , tipo: " ++ tipo p ++ " , imovelid: " ++ show(imovelid p) ++ " )}"
geraFormato_pessoa p = "{Pessoa( nomePessoa: " ++ nomePessoa p ++ " , cpf: " ++ cpf p ++ " , idade: " ++ show(idade p) ++ " )}"
