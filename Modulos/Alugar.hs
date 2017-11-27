module Modulos.Alugar where

alugar :: String -> IO()
alugar cpf = if cpf == "" then alugar_cliente_existente else alugar_novo_cliente


alugar_novo_cliente = do
  putStrLn "Entrou"
  return ()


alugar_cliente_existente = do
  putStrLn " Digite o CPF: "
  return ()
