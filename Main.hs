import Modulos.BancoDeDados
import Modulos.Operacoes
import Modulos.Arvore
import Modulos.Helpers as Helpers
import Modulos.Menus as Menus
import Modulos.Imovel_Controller as Imovel_Controller

main = do
  Modulos.BancoDeDados.criar
  opcao <- Menus.menu_principal
  case opcao of
    1 -> do
      Modulos.Operacoes.criarImovel
      main
    2 -> do
      Modulos.Operacoes.vendaImovel
      main
    3 -> do
      Modulos.Operacoes.alugarImovel
      main
    4 -> do
      main
    5 -> do
      pTipo <- Menus.menu_listando
      case pTipo of
        1 -> Imovel_Controller.listando "Vendas"
        2 -> Imovel_Controller.listando "Alugueis"
      main
    6 -> do
      main
    7 -> return()
