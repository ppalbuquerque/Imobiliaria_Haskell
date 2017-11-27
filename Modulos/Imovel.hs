module Modulos.Imovel (Imovel(..)) where



data Imovel = Imovel { endereco :: String
                     , preco :: Float
                     , descricao :: String
                     , area :: Float
                     , comodos :: Int
                     , tipo :: String
                     , imovelid :: Int
                     } deriving (Show)
