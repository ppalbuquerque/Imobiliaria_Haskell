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
import Modulos.Imovel_Controller as Imovel_Controller


vender = do
  putStrLn $ "Escolha um imovel: "
  pCod <- getLine
  imovel_vendido <- Imovel_Controller.buscar_imovel pCod "Vendas"
  Helpers.saveListToFile "DataBase/Vendas/imoveis_vendidos.txt" imovel_vendido
  imoveis_disponiveis <- Imovel_Controller.buscar_imoveis_except pCod "Vendas"
  Helpers.destroyFile "DataBase/Vendas/imoveis_disponiveis.txt"
  Helpers.saveListToFile "DataBase/Vendas/imoveis_disponiveis.txt" imoveis_disponiveis
  return ()
