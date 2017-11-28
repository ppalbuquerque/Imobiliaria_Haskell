module Modulos.Aluguel_Controller where

import Modulos.Aluguel as Aluguel

buscar_alugueis :: IO [String]
buscar_alugueis = do
  f <- readFile ("DataBase/Alugueis/alugueis.txt")
  let alugueis = lines f
  return alugueis


busca_imovel_cod_as_array :: [String] -> [String]
busca_imovel_cod_as_array [] = [""]
busca_imovel_cod_as_array (x:xs) = if x == "imovel:" then [head xs] else busca_imovel_cod_as_array xs

busca_imovel_cod :: [String] -> String
busca_imovel_cod [] = ""
busca_imovel_cod (x:xs) = if x == "imovel:" then head xs else busca_imovel_cod xs

busca_locatario_cod_as_array :: [String] -> [String]
busca_locatario_cod_as_array [] = [""]
busca_locatario_cod_as_array (x:xs) = if x == "locatario:" then [head xs] else busca_locatario_cod_as_array xs

busca_locatario_cod :: [String] -> String
busca_locatario_cod [] = ""
busca_locatario_cod (x:xs) = if x == "locatario:" then head xs else busca_locatario_cod xs

busca_dataV_as_array :: [String] -> [String]
busca_dataV_as_array [] = [""]
busca_dataV_as_array (x:xs) = if x == "dataV:" then [head xs] else busca_dataV_as_array xs

busca_dataV :: [String] -> String
busca_dataV [] = ""
busca_dataV (x:xs) = if x == "dataV:" then head xs else busca_dataV xs
