module Modulos.Imovel_Controller where


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
