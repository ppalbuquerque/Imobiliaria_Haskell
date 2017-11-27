module Modulos.Pessoa (Pessoa (..)) where

data Pessoa = Pessoa { nomePessoa :: string
                     , cpf :: String
                     , idade :: Int}
