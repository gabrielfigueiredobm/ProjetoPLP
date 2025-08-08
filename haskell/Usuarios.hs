module Usuarios (
   Usuario(..),
   getNome,
   getMensagens,
   getSenha,
   getEspecialidade
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
       especialidade :: String,
       senha :: String,
       triagens :: [String],
       mensagens :: [Mensagem]
     }
   deriving (Show, Eq, Read)

getNome :: Usuario -> String
getNome = nome

getMensagens :: Usuario -> [Mensagem]
getMensagens = mensagens

getEspecialidade :: Usuario -> String
getEspecialidade (Medico _ _ _ especialidade _ _ _) = especialidade
getEspecialidade _ = "Não se aplica"

getSenha :: Usuario -> String
getSenha (Paciente _ _ _ senha _ _) = senha
getSenha (Medico _ _ _ _ senha _ _) = senha
