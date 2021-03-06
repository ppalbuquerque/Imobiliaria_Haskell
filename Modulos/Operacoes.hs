module Modulos.Operacoes where
import Modulos.Arvore as Arvore
import Modulos.Imovel as Imovel
import Modulos.Helpers as Helpers
import Modulos.Venda as Venda
import Modulos.Menus as Menus
import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Alugar as Alugar
import Modulos.Imovel_Controller as Imovel_Controller

import Data.Typeable

criarImovel :: IO()
vendaImovel :: IO()
alugarImovel :: IO()



criarImovel = do
  let imovel = Helpers.dadosImovel
  imovel
  putStr "Imovel criada!!"

vendaImovel = do
    Imovel_Controller.listando "Vendas"
    Venda.vender

alugarImovel = do
    pEscolha <- Menus.menu_alugar_cliente
    case pEscolha of
      1 -> do
        Pessoas_Controller.inserir_pessoa
        Alugar.alugar ""
      2 -> Alugar.alugar ""



 --depositar = do
 	--Saque.depositando
 	--putStr "Deposito Efetuado!!"
