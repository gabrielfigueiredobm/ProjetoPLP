module Haskell.Usuarios where

data Usuario = Paciente { nome :: String, cpf :: String, username :: String, senha :: String, mensagens :: [String]} | Medico { nome :: String, crm :: String, senha :: String, triagens :: [String], mensagens :: [String]}
    deriving (Show, Eq, Read)