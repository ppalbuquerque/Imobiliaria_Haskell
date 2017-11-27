module Modulos.Operacoes where
import Modulos.Saque as Saque
import Modulos.Banco as Banco
import Modulos.Arvore as Arvore
import Modulos.Imovel as Imovel
import Modulos.Helpers as Helpers
import Modulos.Venda as Venda
import Data.Typeable

criarImovel :: IO()
vendaImovel :: IO()
--saldo :: IO()
--sacar :: IO()
--transferir :: IO()
--depositar :: IO()


criarImovel = do
  let imovel = Helpers.dadosImovel
  imovel
  putStr "Imovel criada!!"

vendaImovel = do
    Arvore.listando "Vendas"
    Venda.vender


 --depositar = do
 	--Saque.depositando
 	--putStr "Deposito Efetuado!!"
