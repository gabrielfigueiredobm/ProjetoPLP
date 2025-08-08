module Armazenamento (carregarUsuarios, salvarUsuarios, carregarMensagens, salvarMensagens) where

    import System.Directory (doesFileExist)
    import Usuarios (Usuario)
    import Mensagem (Mensagem)

    carregarUsuarios :: FilePath -> IO [Usuario]
    carregarUsuarios path = do
        existe <- doesFileExist path
        if not existe
            then return []
        else do
            conteudo <- readFile path
            return (read conteudo)

    salvarUsuarios :: FilePath -> [Usuario] -> IO ()
    salvarUsuarios path usuarios =
        writeFile path (show usuarios)

    carregarMensagens :: FilePath -> IO [Mensagem]
    carregarMensagens path = do
        existe <- doesFileExist path
        if not existe
            then return []
        else do
            conteudo <- readFile path
            return (read conteudo)

    salvarMensagens :: FilePath -> [Mensagem] -> IO ()
    salvarMensagens path mensagens =
        writeFile path (show mensagens)