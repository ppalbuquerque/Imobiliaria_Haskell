import Modulos.BancoDeDados
import Modulos.Operacoes
import Modulos.Arvore
import Modulos.Helpers as Helpers
import Modulos.Menus as Menus
import Modulos.Imovel_Controller as Imovel_Controller
import Modulos.Relatorios as Relatorios
import Modulos.Pessoas_Controller as Pessoas_Controller
import Modulos.Aluguel_Controller as Aluguel_Controller

main = do
  Helpers.criar_arquivos
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
      pRelato <- Menus.menu_relatorios
      case pRelato of
        1 -> Relatorios.relatorio_vendas
        2 -> Relatorios.relatorio_alugueis
      main
    5 -> do
      pTipo <- Menus.menu_listando
      pOrd <- Menus.menu_ordenacao_vendas
      case pTipo of
        1 -> Imovel_Controller.listar_imoveis_disponiveis_ord "Vendas" pOrd
        2 -> Imovel_Controller.listar_imoveis_disponiveis_ord "Alugueis" pOrd
      main
    6 -> do
      pOrd <- Menus.menu_ordenacao_clientes
      Pessoas_Controller.listar_pessoas_ord pOrd
      main
    7 -> do
      pOrd <- Menus.menu_ordenacao_aluguel
      Aluguel_Controller.listar_alugueis_ord pOrd
      main
    8 -> do
      Pessoas_Controller.inserir_pessoa
      main
    9 -> return ()  
