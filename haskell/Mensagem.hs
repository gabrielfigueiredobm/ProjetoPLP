module Mensagem (Mensagem(..)) where

data Mensagem = Mensagem {
   remetente   :: String,
   destinatario :: String,
   texto        :: String
} deriving (Show, Eq, Read)
