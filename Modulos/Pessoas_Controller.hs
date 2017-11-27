module Modulos.Pessoas_Controller where

import Modulos.BancoDeDados as BancoDeDados
import Modulos.Pessoa as Pessoa

getInt ::IO Int

getInt = readLn

inserir_pessoa = do
  putStrLn "Nome Cliente: "
  pNome <- getLine
  putStrLn "CPF: "
  pCPF <- getLine
  putStrLn "Idade: "
  pIdade <- readLn

  let pessoa = (Pessoa { nomePessoa = pNome, cpf = pCPF, idade = pIdade })
  BancoDeDados.salvar_pessoa pessoa

listando tipo = do
    f <- readFile ("DataBase/Pessoas/imoveis_disponiveis.txt")
    let fim = lines f
    let pessoas = listar fim
    let ordena = ordenaTuplas imoveis
    putStrLn . unlines . map printImoveis $ imoveis

listar :: [String] -> [(String, String)]
listar [] = []
listar (x:xs) =  [(busca_cpf(words x),busca_nome (words x))] ++ listar xs

busca_cpfs :: [String] -> String
busca_cpfs [] = ""
busca_cpfs (x:xs) = if x == "cpf:" then head xs else busca_cpfs xs

busca_nomes :: [String] -> String
busca_nomes [] = ""
busca_nomes (x:xs) = if x == "nomePessoa:" then head busca_nome_found xs else busca_nomes xs

busca_nome_found :: [String] -> String
busca_nomes_found [] = ""
busca_nomes_found (x:xs) = if x == "," then "" else x ++ " " ++ busca_nomes_found xs
