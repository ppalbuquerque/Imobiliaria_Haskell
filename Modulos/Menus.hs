module Modulos.Menus where

menu_principal :: IO Int
menu_principal = do
  putStrLn "1 - Cadastrar Imovel"
  putStrLn "2 - Vender Imovel"
  putStrLn "3 - Alugar Imovel"
  putStrLn "4 - Relatórios"
  putStrLn "5 - Listar Imoveis"
  putStrLn "7 - Sair"
  putStrLn ""
  putStrLn "Escolha >> "
  opcao <- readLn
  if opcao <= 0 || opcao > 7 then do
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
    menu_listando
  else
    return pTipo
