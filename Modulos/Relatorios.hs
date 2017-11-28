module Modulos.Relatorios where

import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Modulos.Utils as Utils
import Modulos.Imovel_Controller as Imovel_Controller
import Modulos.Aluguel_Controller as Aluguel_Controller
import Modulos.Pessoa as Pessoa
import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Imovel as Imovel
import Modulos.Menus as Menus


-- Seção relatorio Vendas

relatorio_vendas = do
  ord <- Menus.menu_ordenacao_vendas
  agora <- getCurrentTime
  let (ano, mes, dia) = toGregorian $ utctDay agora
  let data_string = Utils.date_to_string (ano, mes, dia)
  let arquivo = ("DataBase/Relatorios/Relatorio_Vendas" ++ data_string)
  writeFile arquivo ""
  appendFile arquivo preparar_cabecalho_vendas
  colocar_elementos_vendas arquivo ord
  appendFile arquivo preparar_fim_vendas
  imoveis_vendidos <-Imovel_Controller.buscar_imoveis_vendidos "Vendas"
  appendFile arquivo (show(somar_total_vendas imoveis_vendidos))

preparar_cabecalho_vendas :: String
preparar_cabecalho_vendas = "-------------------------------- Relatório De Vendas -----------------------------\n | Endereço |  ---  | Preço |  ---  | Descrição |  ---  | Comodos |  ---  | Código | \n\n"

colocar_elementos_vendas :: String -> Int -> IO ()
colocar_elementos_vendas path ord = do
  imoveis_vendidos <- Imovel_Controller.buscar_imoveis_vendidos "Vendas"
  colocar_elementos_vendas_aux path imoveis_vendidos

colocar_elementos_vendas_aux :: String -> [String] -> IO ()
colocar_elementos_vendas_aux path [] = putStrLn "Gravação Concluida"
colocar_elementos_vendas_aux path (x : xs) = do
  appendFile path (gerar_formato_vendas x)
  colocar_elementos_vendas_aux path xs


gerar_formato_vendas :: String -> String
gerar_formato_vendas imovel = " " ++ Imovel_Controller.busca_endereco(words imovel) ++ "          " ++ Imovel_Controller.busca_preco(words imovel) ++ "             " ++ Imovel_Controller.busca_descricao(words imovel) ++ "                " ++ Imovel_Controller.busca_comodos(words imovel) ++ "        " ++ Imovel_Controller.busca_id(words imovel) ++ "\n"

preparar_fim_vendas :: String
preparar_fim_vendas = "------------------------------------ Fim Relatório -------------------------\n Total Vendido = "

somar_total_vendas :: [String] -> Float
somar_total_vendas [] = 0
somar_total_vendas (x : xs) = (read (Imovel_Controller.busca_preco(words x)) :: Float) + somar_total_vendas xs


-- Seção relatório Alugueis
relatorio_alugueis = do
  agora <- getCurrentTime
  let (ano, mes, dia) = toGregorian $ utctDay agora
  let data_string = Utils.date_to_string (ano, mes, dia)
  let arquivo = ("DataBase/Relatorios/Relatorio_Alugueis" ++ data_string)
  writeFile arquivo ""
  appendFile arquivo preparar_cabecalho_alugueis
  colocar_elementos_alugueis arquivo
  appendFile arquivo preparar_fim_vendas
  imoveis_vendidos <- Imovel_Controller.buscar_imoveis_vendidos "Alugueis"
  appendFile arquivo (show(somar_total_alugueis imoveis_vendidos))


preparar_cabecalho_alugueis :: String
preparar_cabecalho_alugueis = "-------------------------------- Relatório De Alugueis -----------------------------\n | Imovel - Cod |  ---  | Locatário - CPF |  --- | Valor Aluguel | -- | Data Pagamento| \n\n"

colocar_elementos_alugueis :: String -> IO ()
colocar_elementos_alugueis path = do
  imoveis_alugados <- Aluguel_Controller.buscar_alugueis
  colocar_elementos_alugueis_aux path imoveis_alugados

colocar_elementos_alugueis_aux :: String -> [String] -> IO ()
colocar_elementos_alugueis_aux path [] = putStrLn "Gravação Concluida"
colocar_elementos_alugueis_aux path (x : xs) = do
  linha <- gerar_formato_aluguel x
  appendFile path linha
  colocar_elementos_alugueis_aux path xs


gerar_formato_aluguel :: String -> IO String
gerar_formato_aluguel aluguel = do
  imovel <- (Imovel_Controller.buscar_imovel_indisponiveis (Aluguel_Controller.busca_imovel_cod (words aluguel)) "Alugueis")
  pessoa <- (Pessoas_Controller.buscar_pessoa (Aluguel_Controller.busca_locatario_cod (words aluguel)))
  let imovel_line = Utils.extract_string imovel
  let pessoa_line = Utils.extract_string pessoa
  let linha = Imovel_Controller.busca_id(words imovel_line) ++ "          " ++ Pessoas_Controller.busca_cpfs(words pessoa_line) ++ "                          " ++ Imovel_Controller.busca_preco(words imovel_line) ++ "                        " ++ Aluguel_Controller.busca_dataV(words aluguel) ++ "\n"
  return linha

preparar_fim_alugueis :: String
preparar_fim_alugueis = "------------------------------------ Fim Relatório -------------------------\n Total Arrecadado = "

somar_total_alugueis :: [String] -> Float
somar_total_alugueis [] = 0
somar_total_alugueis (x : xs) = (read (Imovel_Controller.busca_preco(words x)) :: Float) + somar_total_alugueis xs
