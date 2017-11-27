module Modulos.Aluguel ( Aluguel (..)) where

import Modulos.Imovel as Imovel


data Aluguel = Aluguel { imoverl :: Imovel
                       , data :: String}  
