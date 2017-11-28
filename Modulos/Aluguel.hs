module Modulos.Aluguel ( Aluguel (..)) where

import Modulos.Imovel as Imovel
import Modulos.Pessoa as Pessoa


data Aluguel = Aluguel { imovel :: Imovel
                       , locatario :: Pessoa
                       , dataV :: String} deriving (Show)
