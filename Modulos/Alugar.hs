module Modulos.Alugar where

import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Imovel_Controller as Imovel_Controller
import Modulos.Utils as Utils
import Modulos.Imovel as Imovel
import Modulos.Helpers as Helpers

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
  Imovel_Controller.listando "Alugueis"
  putStrLn "Digite o codigo do imovel: "
  pCod <- getInt
  imovel <- Imovel_Controller.buscar_imovel (show pCod) "Alugueis"
  let imovel_value = Utils.extract_string imovel
  let imovel_instance = Imovel_Controller.instanciar_imovel imovel_value
  let pessoa_instance = Pessoas_Controller.instanciar_pessoa pessoa_value
  print $ endereco imovel_instance
  return ()
