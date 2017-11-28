module Modulos.Alugar where

import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Utils as Utils

alugar :: String -> IO()
alugar cpf = if cpf == "" then alugar_cliente_existente else alugar_novo_cliente


alugar_novo_cliente = do
  putStrLn "Entrou"
  return ()


alugar_cliente_existente = do
  Pessoas_Controller.listando
  putStrLn "Digite o CPF: "
  pCPF <- getInt
  pessoa <- Pessoas_Controller.buscar_pessoa (show pCPF)
  let pessoa_value = Utils.extract_string pessoa
  print $ pessoa_value
  return ()
