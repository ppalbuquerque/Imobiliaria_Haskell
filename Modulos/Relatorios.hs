module Modulos.Relatorios where

import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Modulos.Utils as Utils

relatorio_vendas = do
  agora <- getCurrentTime
  let (ano, mes, dia) = toGregorian $ utctDay agora
  let data_string = Utils.date_to_string (ano, mes, dia)
  print $ data_string
  -- writeFile "DataBase"
