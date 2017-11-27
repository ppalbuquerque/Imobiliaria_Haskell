module Modulos.Pessoa (Pessoa (..)) where

data Pessoa = Pessoa { nomePessoa :: String
                     , cpf :: String
                     , idade :: Int } deriving (Show)
