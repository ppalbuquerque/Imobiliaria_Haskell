module Modulos.Imovel_Controller where

import Modulos.Utils as Utils
import Modulos.Helpers as Helpers
import Modulos.Imovel as Imovel

-- Instanciando a partir de string
instanciar_imovel :: String -> Imovel
instanciar_imovel attr = (Imovel { endereco = busca_endereco(words attr), preco = (read (busca_preco(words attr)) :: Float) , descricao = busca_descricao(words attr), area = (read (busca_area(words attr)) :: Float) , comodos = (read (busca_comodos(words attr)) :: Int), tipo = busca_tipo(words attr),
     imovelid = (read (busca_id(words attr)) :: Int)})
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

busca_preco :: [String] -> String
busca_preco [] = ""
busca_preco (x:xs) = if x == "preco:" then head xs else busca_preco xs

busca_descricao :: [String] -> String
busca_descricao [] = ""
busca_descricao (x:xs) = if x == "descricao:" then head xs else busca_descricao xs

busca_comodos :: [String] -> String
busca_comodos [] = ""
busca_comodos (x:xs) = if x == "comodos:" then head xs else busca_comodos xs

busca_area :: [String] -> String
busca_area [] = ""
busca_area (x:xs) = if x == "area:" then head xs else busca_area xs

busca_tipo :: [String] -> String
busca_tipo [] = ""
busca_tipo (x:xs) = if x == "tipo:" then head xs else busca_tipo xs

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

buscar_imovel_indisponiveis :: String -> String -> IO [String]
buscar_imovel_indisponiveis cod tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_vendidos.txt")
  let fim = lines f
  let imovel = busca_imovel fim cod True
  return imovel

buscar_imoveis_except :: String -> String -> IO [String]
buscar_imoveis_except cod tipo = do
  f <- readFile ("DataBase/" ++ tipo ++ "/imoveis_disponiveis.txt")
  let fim = lines f
  let imoveis = busca_imovel fim cod False
  return imoveis

buscar_imoveis_vendidos :: String -> IO [String]
buscar_imoveis_vendidos tipo = do
  f <- readFile ("DataBase/" ++ tipo ++  "/imoveis_vendidos.txt")
  let imoveis = lines f
  return imoveis

busca_imovel :: [String] -> String -> Bool -> [String]
busca_imovel [] _ _ = [""]
busca_imovel (x : xs) imovel_id flag = if (busca_imovelId (words x) imovel_id) == flag then [x] ++ busca_imovel xs imovel_id flag else busca_imovel xs imovel_id flag

busca_imovelId :: [String] -> String -> Bool
busca_imovelId [] _ = False
busca_imovelId (x : xs) imovel_id = if x == "imovelid:" then (check_id (head xs) imovel_id) else busca_imovelId xs imovel_id

check_id :: String -> String -> Bool
check_id imovelid_txt imovel_id = if imovelid_txt == imovel_id then True else False
