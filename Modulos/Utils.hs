module Modulos.Utils where

ordenaTuplas_pessoas :: [(String, String, String)] -> [(String, String, String)]
ordenaTuplas_pessoas [] = []
ordenaTuplas_pessoas (h : t) = ordenaTuplas_pessoas menores ++ [h] ++ ordenaTuplas_pessoas maiores
  where menores = [ x | x <- t, x <= h ]
        maiores = [ x | x <- t, x > h ]


ordenaTuplas_imoveis :: [(String, String)] -> [(String, String)]
ordenaTuplas_imoveis [] = []
ordenaTuplas_imoveis (h : t) = ordenaTuplas_imoveis menores ++ [h] ++ ordenaTuplas_imoveis maiores
  where menores = [ x | x <- t, x <= h ]
        maiores = [ x | x <- t, x > h ]

extract_string :: [String] -> String
extract_string [] = "Vazio"
extract_string (x : xs) = x

date_to_string :: (Integer, Int, Int) -> String
date_to_string (ano, mes, dia) = (show ano) ++ (show mes) ++ (show dia)
