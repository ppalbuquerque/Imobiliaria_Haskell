module Modulos.Pessoas_Controller where

import Modulos.BancoDeDados as BancoDeDados
import Modulos.Pessoa as Pessoa
import Modulos.Utils as Utils
import Modulos.Helpers as Helpers

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


-- Seção Responsável por fazer as buscas para a listagem

listando = do
    f <- readFile ("DataBase/Pessoas/pessoas.txt")
    let fim = lines f
    let pessoas = listar fim
    let ordena = Utils.ordenaTuplas_pessoas pessoas
    putStrLn . unlines . map Helpers.printPessoas $ pessoas

listar :: [String] -> [(String, String, String)]
listar [] = []
listar (x:xs) =  [(busca_cpfs(words x), busca_nomes (words x), busca_idades (words x))] ++ listar xs

busca_cpfs :: [String] -> String
busca_cpfs [] = ""
busca_cpfs (x:xs) = if x == "cpf:" then head xs else busca_cpfs xs

busca_nomes :: [String] -> String
busca_nomes [] = ""
busca_nomes (x:xs) = if x == "nomePessoa:" then busca_nome_found xs else busca_nomes xs

busca_nome_found :: [String] -> String
busca_nome_found [] = ""
busca_nome_found (x:xs) = if x == "," then "" else x ++ " " ++ busca_nome_found xs

busca_idades :: [String] -> String
busca_idades [] = []
busca_idades (x:xs) = if x == "idade:" then head xs else busca_idades xs

-- Sessão Responsavel para fazer as buscas especificas da pessoa
