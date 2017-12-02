module Modulos.Aluguel_Controller where

import Modulos.Aluguel as Aluguel
import Modulos.Utils as Utils
import Modulos.Helpers as Helpers

buscar_alugueis :: IO [String]
buscar_alugueis = do
  f <- readFile ("DataBase/Alugueis/alugueis.txt")
  let alugueis = lines f
  return alugueis

listar_alugueis_ord :: Int -> IO ()
listar_alugueis_ord ord = do
  f <- readFile ("DataBase/Alugueis/alugueis.txt")
  let alugueis = lines f
  case ord of
    1 -> do
      let alugueis_ord = listar_imovel_c alugueis
      let ordena = Utils.ordenaTuplas_pessoas alugueis_ord
      putStrLn . unlines . map printAluguel_1 $ ordena
    2 -> do
      let alugueis_ord = listar_pessoa_cod alugueis
      let ordena = Utils.ordenaTuplas_pessoas alugueis_ord
      putStrLn . unlines . map printAluguel_2 $ ordena

listar_imovel_c :: [String] -> [(String, String, String)]
listar_imovel_c [] = []
listar_imovel_c (x:xs) =  [(busca_imovel_cod(words x),busca_locatario_cod (words x),busca_dataV(words x))] ++ listar_imovel_c xs

listar_pessoa_cod :: [String] -> [(String, String, String)]
listar_pessoa_cod [] = []
listar_pessoa_cod (x:xs) =  [(busca_locatario_cod(words x),busca_imovel_cod (words x),busca_dataV(words x))] ++ listar_pessoa_cod xs

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
