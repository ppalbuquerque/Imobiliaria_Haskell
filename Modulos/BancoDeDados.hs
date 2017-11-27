module Modulos.BancoDeDados where

import System.Directory
import Control.Monad
import Modulos.Imovel as Imovel
import Modulos.Banco as Banco

-- Cria o diretório DataBase e os arquivos contas.txt
-- e transferencias.txt se não existir.
criar :: IO()
-- Gera formato para ser salvo no arquivo de contas.txt
geraFormato :: Imovel -> String
-- Salva pessoa utilizando appendFile no arquivo de contas.txt
salvar :: Imovel -> IO()
-- Busca uma pessoa no banco de dados

criar = do
    createDirectoryIfMissing True "DataBase"
    let arquivoImoveis = "DataBase/imoveis.txt"

    existeImoveis <- doesFileExist arquivoImoveis
    when (not existeImoveis) (writeFile arquivoImoveis "")

geraFormato p = "{Imovel( endereco: " ++ endereco p ++ " , preco: " ++ show(preco p) ++ " , descricao: " ++ show(area p) ++ " , comodos: " ++ show(comodos p) ++ " , tipo: " ++ tipo p ++ " , imovelid: " ++ show(imovelid p) ++ " )}"

salvar p = do
  appendFile "DataBase/imoveis.txt" (geraFormato p ++ "\n")
  putStr "Banco de dados atualizado!\n"
