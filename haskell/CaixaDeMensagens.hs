module CaixaDeMensagens (
    enviarMensagem,
    verMensagens,
    apagarMensagem
) where

import Usuarios (Usuario(..), getNome, getUsername)
import Mensagem (Mensagem(..))

enviarMensagem :: Usuario -> Usuario -> String -> [Mensagem] -> [Mensagem]
enviarMensagem remetente destinatario texto caixa =
  caixa ++ [Mensagem (getNome remetente) (getUsername destinatario) texto]

verMensagens :: [Mensagem] -> [String]
verMensagens caixa =
  [ "[" ++ show i ++ "]" ++ texto msg | (i, msg) <- zip [1..] caixa]

apagarMensagem :: [Mensagem] -> Int -> [Mensagem]
apagarMensagem caixa i
  | i < 0 || i >= length caixa = caixa
  | otherwise =
      let (antes, depois) = splitAt i caixa
      in antes ++ drop 1 depois
