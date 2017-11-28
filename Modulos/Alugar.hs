module Modulos.Alugar where

import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Imovel_Controller as Imovel_Controller
import Modulos.Utils as Utils
import Modulos.Imovel as Imovel
import Modulos.Helpers as Helpers
import Modulos.Aluguel as Aluguel
import Modulos.BancoDeDados as BancoDeDados

alugar :: String -> IO()
alugar cpf = if cpf == "" then alugar_cliente_existente else alugar_novo_cliente cpf

alugar_cliente_existente = do
  Pessoas_Controller.listando
  putStrLn "Digite o CPF: "
  pCPF <- getInt
  alugar_novo_cliente pCPF
  return ()


alugar_novo_cliente pCPF = do
  pessoa <- Pessoas_Controller.buscar_pessoa (show pCPF)
  let pessoa_value = Utils.extract_string pessoa
  Imovel_Controller.listando "Alugueis"
  putStrLn "Digite o codigo do imovel: "
  pCod <- getInt
  imovel <- Imovel_Controller.buscar_imovel (show pCod) "Alugueis"
  let imovel_value = Utils.extract_string imovel
  let imovel_instance = Imovel_Controller.instanciar_imovel imovel_value
  let pessoa_instance = Pessoas_Controller.instanciar_pessoa pessoa_value
  putStrLn "Data Do Vencimento Do Aluguel (dd/mm/aaaa): "
  pData <- getLine
  let aluguel = Aluguel { imovel = imovel_instance, locatario = pessoa_instance, dataV = pData}
  BancoDeDados.salvar_aluguel aluguel
  imovel_alugado <- Imovel_Controller.buscar_imovel (show (imovelid imovel_instance)) "Alugueis"
  Helpers.saveListToFile "DataBase/Alugueis/imoveis_vendidos.txt" imovel_alugado
  imoveis_disponiveis <- Imovel_Controller.buscar_imoveis_except (show (imovelid imovel_instance)) "Alugueis"
  Helpers.destroyFile "DataBase/Alugueis/imoveis_disponiveis.txt"
  Helpers.saveListToFile "DataBase/Alugueis/imoveis_disponiveis.txt" imoveis_disponiveis
  return ()
