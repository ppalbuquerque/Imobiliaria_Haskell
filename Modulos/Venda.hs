module Modulos.Venda where
import Prelude
import System.IO
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.IO.Error
import Modulos.Arvore
import Modulos.Helpers as Helpers

vender = do
  putStrLn $ "Escolha um imovel: "
  pCod <- getLine
  f <- readFile "DataBase/Vendas/imoveis_disponiveis.txt"
  let fim = lines f
  let imoveis_disponiveis = busca_imovel fim pCod False
  Helpers.destroyFile "DataBase/Vendas/imoveis_disponiveis.txt"
  Helpers.saveListToFile "DataBase/Vendas/imoveis_disponiveis.txt" imoveis_disponiveis
  let imovel_vendido = busca_imovel fim pCod True
  Helpers.saveListToFile "DataBase/Vendas/imoveis_vendidos.txt" imovel_vendido
  return ()

busca_imovel :: [String] -> String -> Bool -> [String]
busca_imovel [] _ _ = [""]
busca_imovel (x : xs) imovel_id flag = if (busca_imovelId (words x) imovel_id) == flag then [x] ++ busca_imovel xs imovel_id flag else busca_imovel xs imovel_id flag

busca_imovelId :: [String] -> String -> Bool
busca_imovelId [] _ = False
busca_imovelId (x : xs) imovel_id = if x == "imovelid:" then (check_id (head xs) imovel_id) else busca_imovelId xs imovel_id

check_id :: String -> String -> Bool
check_id imovelid_txt imovel_id = if imovelid_txt == imovel_id then True else False
check_id _ _ = True
