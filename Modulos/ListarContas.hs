module Modulos.ListarContas where
--import Modulos.Pessoa as Pessoa
listarContas ::  IO()
listarContas = do
  f <- readFile "/Users/luishenriquesilva/Desktop/hell_bank-master/DataBase/contas.txt"
  print $ lines f
