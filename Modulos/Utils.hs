module Modulos.Utils where

ordenaTuplas_pessoas :: [(String, String, String)] -> [(String, String, String)]
ordenaTuplas_pessoas [] = []
ordenaTuplas_pessoas (h : t) = ordenaTuplas_pessoas menores ++ [h] ++ ordenaTuplas_pessoas maiores
  where menores = [ x | x <- t, x <= h ]
        maiores = [ x | x <- t, x > h ]

extract_string :: [String] -> String
extract_string (x : xs) = x
