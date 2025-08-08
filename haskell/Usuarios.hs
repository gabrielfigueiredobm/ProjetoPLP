module Usuarios (
   Usuario(..),
   getNome,
   getMensagens,
   getSenha,
   getEspecialidade,
   getUsername
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
   | Admin {
       username :: String,
       senha :: String
     }
   deriving (Show, Eq, Read)

getNome :: Usuario -> String
getNome (Paciente nome _ _ _ _ _) = nome
getNome (Medico nome _ _ _ _ _ _) = nome
getNome (Admin username _) = "Administrador"

getUsername :: Usuario -> String
getUsername (Paciente _ _ username _ _ _) = username
getUsername (Medico _ _ username _ _ _ _) = username
getUsername (Admin username _) = username

getMensagens :: Usuario -> [Mensagem]
getMensagens = mensagens

getEspecialidade :: Usuario -> String
getEspecialidade (Medico _ _ _ especialidade _ _ _) = especialidade
getEspecialidade _ = "Não se aplica"

getSenha :: Usuario -> String
getSenha (Paciente _ _ _ senha _ _) = senha
getSenha (Medico _ _ _ _ senha _ _) = senha
getSenha (Admin _ senha) = senha
