module Modulos.Menus where

menu_principal :: IO Int
menu_principal = do
  putStrLn "1 - Cadastrar Imovel"
  putStrLn "2 - Vender Imovel"
  putStrLn "3 - Alugar Imovel"
  putStrLn "4 - Relatórios"
  putStrLn "5 - Listar Imoveis"
  putStrLn "6 - Listar Clientes"
  putStrLn "7 - Listar Alugueis"
  putStrLn "8 - Sair"
  putStrLn ""
  putStrLn "Escolha >> "
  opcao <- readLn
  if opcao <= 0 || opcao > 8 then do
    putStrLn "Opção inválida"
    menu_principal
  else
    return opcao


menu_listando :: IO Int
menu_listando = do
  putStrLn "1 - Para Venda"
  putStrLn "2 - Para Aluguel"
  pTipo <- readLn
  if pTipo <= 0 || pTipo > 2 then do
    putStrLn "Opção inválida"
    menu_listando
  else
    return pTipo


menu_alugar_cliente :: IO Int
menu_alugar_cliente = do
  putStrLn "1 - Novo Cliente"
  putStrLn "2 - Cliente Existente"
  pTipo <- readLn
  if pTipo <= 0 || pTipo > 2 then do
    putStrLn "Opção inválida"
    menu_alugar_cliente
  else
    return pTipo

menu_relatorios :: IO Int
menu_relatorios = do
  putStrLn "1 - Relatório De Vendas"
  putStrLn "2 - Relatório De Alugueis"
  pTipo <- readLn
  if pTipo <= 0 || pTipo > 2 then do
    putStrLn "Opção inválida"
    menu_relatorios
  else
    return pTipo

menu_ordenacao_vendas :: IO Int
menu_ordenacao_vendas = do
  putStrLn "1 - Endereço"
  putStrLn "2 - Preço"
  putStrLn "3 - Código"
  pEscolha <- readLn
  if pEscolha <= 0 || pEscolha > 3 then do
    putStrLn "Opção inválida"
    menu_ordenacao_vendas
  else
    return pEscolha
