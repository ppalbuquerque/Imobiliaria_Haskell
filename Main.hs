import Modulos.BancoDeDados
import Modulos.Operacoes
import Modulos.Deposito
import Modulos.Saque
import Modulos.ListarContas
import Modulos.Arvore
import Modulos.Helpers as Helpers
import Modulos.Menus as Menus

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
      Modulos.Saque.sacando
      Modulos.Saque.transfere
      main
    4 -> do
      Modulos.Deposito.depositando
      Modulos.Deposito.transfere
      main
    5 -> do
      pTipo <- Menus.menu_listando
      case pTipo of
        1 -> Modulos.Arvore.listando "Vendas"
        2 -> Modulos.Arvore.listando "Alugueis"
      main
    6 -> do
      Modulos.Deposito.juros
      Modulos.Deposito.transfere
      main
    7 -> return()
