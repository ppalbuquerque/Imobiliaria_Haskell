module Modulos.Imovel_Controller where

import Modulos.Utils as Utils
import Modulos.Helpers as Helpers

-- Seção referente a listagem de imoveis

listando :: String -> IO()
listando tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let imoveis = listar fim
  let ordena = Utils.ordenaTuplas_imoveis imoveis
  putStrLn . unlines . map printImoveis $ imoveis


listar :: [String] -> [(String, String)]
listar [] = []
listar (x:xs) =  [(busca_id(words x),busca_endereco (words x))] ++ listar xs

busca_id :: [String] -> String
busca_id [] = ""
busca_id (x:xs) = if x == "imovelid:" then head xs else busca_id xs

busca_endereco :: [String] -> String
busca_endereco [] = ""
busca_endereco (x:xs) = if x == "endereco:" then busca_endereco_found xs else busca_endereco xs

busca_endereco_found :: [String] -> String
busca_endereco_found [] = ""
busca_endereco_found (x:xs) = if x == "," then "" else x ++ " " ++ busca_endereco_found xs


-- Seção referente a busca de imovel

buscar_imovel :: String -> String -> IO [String]
buscar_imovel cod tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let imovel = busca_imovel fim cod True
  return imovel

buscar_imoveis_except :: String -> String -> IO [String]
buscar_imoveis_except cod tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let imoveis = busca_imovel fim cod False
  return imoveis

busca_imovel :: [String] -> String -> Bool -> [String]
busca_imovel [] _ _ = [""]
busca_imovel (x : xs) imovel_id flag = if (busca_imovelId (words x) imovel_id) == flag then [x] ++ busca_imovel xs imovel_id flag else busca_imovel xs imovel_id flag

busca_imovelId :: [String] -> String -> Bool
busca_imovelId [] _ = False
busca_imovelId (x : xs) imovel_id = if x == "imovelid:" then (check_id (head xs) imovel_id) else busca_imovelId xs imovel_id

check_id :: String -> String -> Bool
check_id imovelid_txt imovel_id = if imovelid_txt == imovel_id then True else False
