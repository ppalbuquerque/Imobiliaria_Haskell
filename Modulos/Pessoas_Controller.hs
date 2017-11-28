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

buscar_pessoa :: String -> IO [String]
buscar_pessoa cpf = do
  f <- readFile "DataBase/Pessoas/pessoas.txt"
  let fim = lines f
  let pessoa = busca_pessoa fim cpf True
  return pessoa

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

busca_pessoa :: [String] -> String -> Bool -> [String]
busca_pessoa [] _ _ = [""]
busca_pessoa (x : xs) pessoa_cpf flag = if (busca_pessoaCpf (words x) pessoa_cpf) == flag then [x] ++ busca_pessoa xs pessoa_cpf flag else busca_pessoa xs pessoa_cpf flag

busca_pessoaCpf :: [String] -> String -> Bool
busca_pessoaCpf [] _ = False
busca_pessoaCpf (x : xs) pessoa_cpf = if x == "cpf:" then (check_id (head xs) pessoa_cpf) else busca_pessoaCpf xs pessoa_cpf

check_id :: String -> String -> Bool
check_id cpf_txt pessoa_cpf = if cpf_txt == pessoa_cpf then True else False
