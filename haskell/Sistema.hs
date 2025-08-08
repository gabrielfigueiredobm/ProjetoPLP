module Sistema (runSistema) where

import Usuarios (Usuario(..), getNome, getSenha, getEspecialidade, getUsername)
import Mensagem (Mensagem(..))

import Triagem as T
import CaixaDeMensagens as CM

import Armazenamento

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.List (nub, intercalate, find)
import Data.Char (isSpace, toLower)
import Control.Monad (when)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

listaSintomasDisponiveis :: [String]
listaSintomasDisponiveis =
  let todosSintomas = concatMap snd T.doencas
  in  nub todosSintomas

lerSintomas :: IO [String]
lerSintomas = do
  putStrLn "Digite os sintomas separados por vírgula (exemplo: Febre, Tosse):"
  putStrLn "Sintomas possíveis:"
  putStrLn (formatarListaSintomas listaSintomasDisponiveis)
  line <- getLine
  let sintomas = map (trim) $ splitOn ',' line
  return sintomas

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s =
  let (pre, suf) = break (== c) s
  in pre : case suf of
       [] -> []
       (_:rest) -> splitOn c rest

formatarListaSintomas :: [String] -> String
formatarListaSintomas [] = ""
formatarListaSintomas sintomas = 
  intercalate "\n" $ map formataLinha (chunksOf 5 sintomas)
    where
      formataLinha :: [String] -> String
      formataLinha linha = ". " ++ (intercalate ", " linha)

      chunksOf :: Int -> [a] -> [[a]]
      chunksOf _ [] = []
      chunksOf n xs = take n xs : chunksOf n (drop n xs)

encontrarUsuario :: String -> [Usuario] -> Maybe Usuario
encontrarUsuario nome = find (\u -> getUsername u == nome)

lerEntradaNaoVazia :: String -> IO String
lerEntradaNaoVazia prompt = do
  putStr prompt
  entrada <- getLine
  if trim entrada == ""
    then do
      putStrLn "A entrada não pode ser vazia. Por favor, tente novamente."
      lerEntradaNaoVazia prompt
    else
      return entrada

cadastrarPaciente :: IO Usuario
cadastrarPaciente = do
  putStrLn "Cadastro de Paciente"
  nome <- lerEntradaNaoVazia "Nome: "
  cpf <- lerEntradaNaoVazia "CPF: "
  username <- lerEntradaNaoVazia "Username: "
  senha <- lerEntradaNaoVazia "Senha: "
  return $ Paciente nome cpf username senha [] []

listarPacientes :: [Usuario] -> [Usuario]
listarPacientes = filter isPaciente
  where
    isPaciente (Paciente _ _ _ _ _ _) = True
    isPaciente _ = False

cadastrarMedico :: IO Usuario
cadastrarMedico = do
  putStrLn "Cadastro de Médico"
  nome <- lerEntradaNaoVazia "Nome: "
  crm <- lerEntradaNaoVazia "CRM: "
  username <- lerEntradaNaoVazia "Username: "
  especialidade <- lerEntradaNaoVazia "Especialidade: "
  senha <- lerEntradaNaoVazia "Senha: "
  return $ Medico nome crm username especialidade senha [] []

listarMedicos :: [Usuario] -> [Usuario]
listarMedicos = filter isMedico
  where
    isMedico (Medico _ _ _ _ _ _ _) = True
    isMedico _ = False

removerUsuario :: String -> [Usuario] -> [Usuario]
removerUsuario usernameParaApagar = filter (\u -> getUsername u /= usernameParaApagar)

enviarMensagemParaCaixa :: Usuario -> Usuario -> [Mensagem] -> IO [Mensagem]
enviarMensagemParaCaixa remetente destinatario caixa = do
  putStrLn $ "Digite o texto da mensagem para " ++ getNome destinatario ++ ":"
  texto <- getLine
  let novaCaixa = CM.enviarMensagem remetente destinatario texto caixa
  putStrLn $ "Mensagem enviada para " ++ getNome destinatario ++ ": " ++ texto
  return novaCaixa

usernameJaExiste :: String -> [Usuario] -> Bool
usernameJaExiste usernameDesejado usuarios =
  any (\u -> getUsername u == usernameDesejado) usuarios

crmJaExiste :: String -> [Usuario] -> Bool
crmJaExiste crmDesejado usuarios =
  any (\u -> case u of
               Medico { crm = c } -> c == crmDesejado
               _ -> False) usuarios

validarUsuario :: Usuario -> [Usuario] -> IO (Maybe Usuario)
validarUsuario usuario usuarios = do
  if usernameJaExiste (getUsername usuario) usuarios
    then do
      putStrLn "Este nome de usuário já está em uso. Por favor, escolha outro."
      putStr "Novo username: "
      novoUsername <- getLine
      validarUsuario (usuario { username = novoUsername }) usuarios
    else if case usuario of
              Medico { crm = c } -> crmJaExiste c usuarios
              _ -> False
         then do
           putStrLn "Este CRM já está cadastrado. Por favor, informe outro."
           putStr "Novo CRM: "
           novoCrm <- getLine
           case usuario of
             Medico{nome=n, crm=_, username=u, especialidade=e, senha=s, triagens=t, mensagens=m} ->
               validarUsuario (Medico n novoCrm u e s t m) usuarios
             _ -> validarUsuario usuario usuarios
         else do
           putStrLn $ "Usuário " ++ getNome usuario ++ " cadastrado com sucesso!"
           return (Just usuario)

processarCadastro :: String -> [Usuario] -> IO (Maybe Usuario)
processarCadastro tipo usuarios = do
  novoUsuario <- if tipo == "Paciente" then cadastrarPaciente else cadastrarMedico
  validarUsuario novoUsuario usuarios

menuMensagensPaciente :: Usuario -> [Mensagem] -> [Usuario] -> IO ([Usuario], [Mensagem])
menuMensagensPaciente usuario caixa usuarios = do
  putStrLn "\n--- Gerenciar Mensagens ---"
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Ver caixa de mensagens"
  putStrLn "2 - Responder a uma mensagem (de um médico)"
  putStrLn "3 - Enviar uma nova mensagem (para um médico)"
  putStrLn "4 - Apagar uma mensagem"
  putStrLn "5 - Voltar"
  putStr "Opção: "
  opc <- getLine
  case opc of
    "1" -> do
      putStrLn "Caixa de mensagens recebidas:"
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      mapM_ putStrLn (CM.verMensagens recebidas)
      menuMensagensPaciente usuario caixa usuarios
    "2" -> do
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      if null recebidas
        then do
          putStrLn "Você não tem mensagens para responder."
          menuMensagensPaciente usuario caixa usuarios
        else do
          putStrLn "Escolha a mensagem que deseja responder (digite o número):"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - De: " ++ remetente m ++ ", Texto: " ++ texto m) (zip [1..] recebidas)
          putStr "Opção: "
          op <- getLine
          let maybeIndice = reads op :: [(Int, String)]
          case maybeIndice of
            [(idx, _)] | idx >= 1 && idx <= length recebidas -> do
              let mensagemOriginal = recebidas !! (idx - 1)
              let remetenteOriginal = remetente mensagemOriginal
              case encontrarUsuario remetenteOriginal usuarios of
                Just medico -> do
                  novaCaixa <- enviarMensagemParaCaixa usuario medico caixa
                  menuMensagensPaciente usuario novaCaixa usuarios
                Nothing -> do
                  putStrLn "Erro: Médico não encontrado."
                  menuMensagensPaciente usuario caixa usuarios
            _ -> do
              putStrLn "Opção inválida."
              menuMensagensPaciente usuario caixa usuarios
    "3" -> do
      let medicos = [u | u@(Medico {}) <- usuarios, getNome u /= getNome usuario]
      if null medicos
        then do
          putStrLn "Não há médicos cadastrados para enviar mensagem."
          menuMensagensPaciente usuario caixa usuarios
        else do
          putStrLn "Escolha o destinatario médico digitando o número correspondente:"
          mapM_ (\(i,u) -> putStrLn $ show i ++ " - " ++ getNome u ++ " - Especialidade: " ++ getEspecialidade u ++ ".") (zip [1..] medicos)
          putStr "Opção: "
          op <- getLine
          let maybeIndice = reads op :: [(Int, String)]
          case maybeIndice of
            [(idx, _)] | idx >= 1 && idx <= length medicos -> do
              let destinatario = medicos !! (idx -1)
              novaCaixa <- enviarMensagemParaCaixa usuario destinatario caixa
              menuMensagensPaciente usuario novaCaixa usuarios
            _ -> do
              putStrLn "Opção inválida."
              menuMensagensPaciente usuario caixa usuarios
    "4" -> do
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      if null recebidas
        then do
          putStrLn "Sua caixa de mensagens está vazia."
          menuMensagensPaciente usuario caixa usuarios
        else do
          putStrLn "Escolha a mensagem que deseja apagar (digite o número):"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - De: " ++ remetente m ++ ", Texto: " ++ texto m) (zip [1..] recebidas)
          putStr "Número da mensagem: "
          op <- getLine
          let maybeIndice = reads op :: [(Int, String)]
          case maybeIndice of
            [(idx, _)] | idx >= 1 && idx <= length recebidas -> do
              let novaCaixa = CM.apagarMensagem caixa (idx - 1)
              putStrLn "Mensagem apagada com sucesso!"
              menuMensagensPaciente usuario novaCaixa usuarios
            _ -> do
              putStrLn "Número inválido."
              menuMensagensPaciente usuario caixa usuarios
    "5" -> do
      putStrLn "Voltando ao menu principal..."
      menuPaciente usuario caixa usuarios
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuMensagensPaciente usuario caixa usuarios

iniciarMenuPaciente :: Usuario -> [Mensagem] -> [Usuario] -> IO ([Usuario], [Mensagem])
iniciarMenuPaciente usuario caixa usuarios = do
  putStrLn $ "\nBem-vindo(a), " ++ getNome usuario ++ "!"
  menuPaciente usuario caixa usuarios

menuPaciente :: Usuario -> [Mensagem] -> [Usuario] -> IO ([Usuario], [Mensagem])
menuPaciente usuario caixa usuarios = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Fazer triagem (informar sintomas)"
  putStrLn "2 - Gerenciar mensagens"
  putStrLn "3 - Sair"
  putStr "Opção: "
  opc <- getLine
  case opc of
    "1" -> do
      sintomas <- lerSintomas
      putStrLn "Resultados da Triagem:"
      let resultados = T.triagem sintomas
      putStr (T.formataSaida resultados)
      let novoUsuario = usuario { ultimosResultadosTriagem = resultados }
      let novosUsuarios = map (\u -> if getNome u == getNome usuario then novoUsuario else u) usuarios
      menuPaciente novoUsuario caixa novosUsuarios
    "2" -> do
      menuMensagensPaciente usuario caixa usuarios
    "3" -> do
      putStrLn "Saindo do menu do usuário..."
      return (usuarios, caixa)
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuPaciente usuario caixa usuarios

iniciarMenuMedico :: Usuario -> [Mensagem] -> [Usuario] -> IO ([Usuario], [Mensagem])
iniciarMenuMedico usuario caixa usuarios = do
  putStrLn $ "\nBem-vindo(a), Dr(a). " ++ getNome usuario ++ "!"
  menuMedico usuario caixa usuarios

menuMedico :: Usuario -> [Mensagem] -> [Usuario] -> IO ([Usuario], [Mensagem])
menuMedico usuario caixa usuarios = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Ver caixa de mensagens"
  putStrLn "2 - Responder a uma mensagem"
  putStrLn "3 - Apagar uma mensagem"
  putStrLn "4 - Sair"
  putStr "Opção: "
  opc <- getLine
  case opc of
    "1" -> do
      putStrLn "Caixa de mensagens recebidas:"
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      mapM_ putStrLn (CM.verMensagens recebidas)
      menuMedico usuario caixa usuarios
    "2" -> do
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      if null recebidas
        then do
          putStrLn "Você não tem mensagens para responder."
          menuMedico usuario caixa usuarios
        else do
          putStrLn "Escolha a mensagem que deseja responder (digite o número):"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - De: " ++ remetente m ++ ", Texto: " ++ texto m) (zip [1..] recebidas)
          putStr "Opção: "
          op <- getLine
          let maybeIndice = reads op :: [(Int, String)]
          case maybeIndice of
            [(idx, _)] | idx >= 1 && idx <= length recebidas -> do
              let mensagemOriginal = recebidas !! (idx - 1)
              let remetenteOriginal = remetente mensagemOriginal
              case find (\u -> getNome u == remetenteOriginal) usuarios of
                Just paciente@(Paciente { ultimosResultadosTriagem = resultados }) -> do
                    when (not (null resultados)) $ do
                        putStrLn "\n--- Últimos Resultados da Triagem do Paciente ---"
                        putStrLn (T.formataSaida resultados)
                        putStrLn "---------------------------------------------"
                    novaCaixa <- enviarMensagemParaCaixa usuario paciente caixa
                    menuMedico usuario novaCaixa usuarios
                Just _ -> do
                    putStrLn "Erro: Remetente é um médico, não um paciente."
                    menuMedico usuario caixa usuarios
                Nothing -> do
                    putStrLn "Erro: Paciente não encontrado."
                    menuMedico usuario caixa usuarios
            _ -> do
              putStrLn "Opção inválida."
              menuMedico usuario caixa usuarios
    "3" -> do 
      let recebidas = filter (\m -> destinatario m == getNome usuario) caixa
      if null recebidas
        then do
          putStrLn "Sua caixa de mensagens está vazia."
          menuMedico usuario caixa usuarios
        else do
          putStrLn "Escolha a mensagem que deseja apagar (digite o número):"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - De: " ++ remetente m ++ ", Texto: " ++ texto m) (zip [1..] recebidas)
          putStr "Número da mensagem: "
          op <- getLine
          let maybeIndice = reads op :: [(Int, String)]
          case maybeIndice of
            [(idx, _)] | idx >= 1 && idx <= length recebidas -> do
              let novaCaixa = CM.apagarMensagem caixa (idx - 1)
              putStrLn "Mensagem apagada com sucesso!"
              menuMedico usuario novaCaixa usuarios
            _ -> do
              putStrLn "Número inválido."
              menuMedico usuario caixa usuarios
    "4" -> do
      putStrLn "Saindo do menu do médico..."
      return (usuarios, caixa)
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuMedico usuario caixa usuarios

adminMenu :: [Usuario] -> [Mensagem] -> IO ([Usuario], [Mensagem])
adminMenu usuarios caixa = do
  putStrLn "\n--- MENU DO ADMINISTRADOR ---"
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Listar todos os usuários"
  putStrLn "2 - Apagar qualquer cadastro (sem senha)"
  putStrLn "3 - Sair"
  putStr "Opção: "
  opc <- getLine
  case opc of
    "1" -> do
      putStrLn "\n--- LISTA COMPLETA DE USUÁRIOS ---"
      if null usuarios
        then putStrLn "Nenhum usuário cadastrado."
        else mapM_ (\u -> putStrLn $ getNome u ++ " | Username: " ++ getUsername u) usuarios
      adminMenu usuarios caixa
    "2" -> do
      putStrLn "--- Apagar um Cadastro ---"
      putStr "Digite o nome de usuário do cadastro que deseja apagar: "
      usernameParaApagar <- getLine
      case encontrarUsuario usernameParaApagar usuarios of
        Just usuarioParaApagar -> do
          let novosUsuarios = removerUsuario usernameParaApagar usuarios
          putStrLn $ "Cadastro de '" ++ getNome usuarioParaApagar ++ "' apagado com sucesso."
          adminMenu novosUsuarios caixa
        Nothing -> do
          putStrLn "Usuário não encontrado."
          adminMenu usuarios caixa
    "3" -> do
      putStrLn "Saindo do menu do administrador..."
      return (usuarios, caixa)
    _ -> do
      putStrLn "Opção inválida."
      adminMenu usuarios caixa

runSistema :: IO ()
runSistema = do
  usuarios <- carregarUsuarios "database/usuarios.txt"
  caixa    <- carregarMensagens "database/mensagens.txt"
  loop usuarios caixa
  where
    loop :: [Usuario] -> [Mensagem] -> IO ()
    loop usuarios caixa = do
      putStrLn "\nBem-vindo ao nosso sistema de telemedicina!"
      putStrLn "Escolha uma opção:"
      putStrLn "1 - Cadastrar novo usuário"
      putStrLn "2 - Fazer login"
      putStrLn "3 - Listar cadastros"
      putStrLn "4 - Apagar um cadastro"
      putStrLn "5 - Sair"
      putStr "Opção: "
      opc <- getLine
      case opc of
        "1" -> do
          putStrLn "Você deseja se cadastrar como:"
          putStrLn "1 - Paciente"
          putStrLn "2 - Médico"
          putStr "Opção: "
          tipo <- getLine
          case tipo of
            "1" -> do
              maybeNovoUsuario <- processarCadastro "Paciente" usuarios
              case maybeNovoUsuario of
                Just novoUsuario -> do
                  let novaLista = novoUsuario : usuarios
                  salvarUsuarios "database/usuarios.txt" novaLista
                  loop novaLista caixa
                Nothing -> loop usuarios caixa
            "2" -> do
              maybeNovoUsuario <- processarCadastro "Medico" usuarios
              case maybeNovoUsuario of
                Just novoUsuario -> do
                  let novaLista = novoUsuario : usuarios
                  salvarUsuarios "database/usuarios.txt" novaLista
                  loop novaLista caixa
                Nothing -> loop usuarios caixa
            _ -> do
              putStrLn "Opção inválida. Retornando ao menu principal sem cadastro."
              loop usuarios caixa
        "2" -> do
          putStr "Digite seu nome de usuário: "
          nomeLogin <- getLine
          putStr "Digite sua senha: "
          senhaLogin <- getLine
          if nomeLogin == "Adm" && senhaLogin == "PIrs1234@"
            then do
              putStrLn "Login bem-sucedido como Administrador!"
              (novosUsuarios, novaCaixa) <- adminMenu usuarios caixa
              loop novosUsuarios novaCaixa
            else case encontrarUsuario nomeLogin usuarios of
              Just usuario -> do
                if getSenha usuario == senhaLogin
                  then do
                    putStrLn $ "Login bem-sucedido como " ++ getNome usuario
                    (novosUsuarios, novaCaixa) <- case usuario of
                      Paciente{} -> iniciarMenuPaciente usuario caixa usuarios
                      Medico{}   -> iniciarMenuMedico usuario caixa usuarios
                    loop novosUsuarios novaCaixa
                  else do
                    putStrLn "Senha incorreta."
                    loop usuarios caixa
              Nothing -> do
                putStrLn "Usuário não encontrado. Faça o cadastro primeiro."
                loop usuarios caixa
        "3" -> do 
          putStrLn "--- Listar Cadastros ---"
          putStrLn "Escolha o tipo de cadastro a ser listado:"
          putStrLn "1 - Pacientes"
          putStrLn "2 - Médicos"
          putStr "Opção: "
          opcListar <- getLine
          case opcListar of
            "1" -> do
              putStrLn "\n--- Lista de Pacientes ---"
              let pacientes = listarPacientes usuarios
              if null pacientes
                then putStrLn "Nenhum paciente cadastrado."
                else mapM_ (\p -> putStrLn $ "Nome: " ++ getNome p ++ ", Username: " ++ getUsername p) pacientes
              loop usuarios caixa
            "2" -> do
              putStrLn "\n--- Lista de Médicos ---"
              let medicos = listarMedicos usuarios
              if null medicos
                then putStrLn "Nenhum médico cadastrado."
                else mapM_ (\medico -> putStrLn $ "Dr(a). " ++ getNome medico ++ " - Especialidade: " ++ getEspecialidade medico ++ ", Username: " ++ getUsername medico) medicos
              loop usuarios caixa
            _ -> do
              putStrLn "Opção inválida."
              loop usuarios caixa
        "4" -> do
          putStrLn "--- Apagar um Cadastro ---"
          putStr "Digite o nome de usuário do cadastro que deseja apagar: "
          usernameParaApagar <- getLine
          case encontrarUsuario usernameParaApagar usuarios of
            Just usuarioParaApagar -> do
              putStr "Digite a senha para confirmar a exclusão: "
              senhaConfirmacao <- getLine
              if getSenha usuarioParaApagar == senhaConfirmacao
                then do
                  let novosUsuarios = removerUsuario usernameParaApagar usuarios
                  putStrLn $ "Cadastro de '" ++ usernameParaApagar ++ "' apagado com sucesso."
                  loop novosUsuarios caixa
              else do
                putStrLn "Senha incorreta. A exclusão foi cancelada."
                loop usuarios caixa
            Nothing -> do
              putStrLn "Usuário não encontrado."
              loop usuarios caixa
        "5" -> do
          salvarUsuarios "database/usuarios.txt" usuarios
          salvarMensagens "database/mensagens.txt" caixa
          putStrLn "Encerrando o sistema. Até logo!"
        _ -> do
          putStrLn "Opção inválida."
          loop usuarios caixa
