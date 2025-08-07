module Haskell.Usuarios (
   Usuario(..),
   getNome,
   getMensagens
) where

import Mensagem (Mensagem)
import Triagem (Doenca, triagem)

data Usuario
   = Paciente{
       nome :: String,
       cpf :: String,
       username :: String,
       senha :: String,
       mensagens :: [Mensagem],
       ultimosResultadosTriagem :: [(Doenca, Int, Int)]
     }
   | Medico {
       nome :: String,
       crm :: String,
       username :: String,
       senha :: String,
       triagens :: [String],
       mensagens :: [Mensagem]
     }
   deriving (Show, Eq, Read)

getNome :: Usuario -> String
getNome = nome

getMensagens :: Usuario -> [Mensagem]
getMensagens = mensagens
