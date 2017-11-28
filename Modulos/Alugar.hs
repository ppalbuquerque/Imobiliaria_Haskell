module Modulos.Alugar where

import Modulos.Pessoas_Controller as Pessoas_Controller


alugar :: String -> IO()
alugar cpf = if cpf == "" then alugar_cliente_existente else alugar_novo_cliente


alugar_novo_cliente = do
  putStrLn "Entrou"
  return ()


alugar_cliente_existente = do
  Pessoas_Controller.listando
  putStrLn "Digite o CPF: "
  pCPF <- getInt
  return ()
