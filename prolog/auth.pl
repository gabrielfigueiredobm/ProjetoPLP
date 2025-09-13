:- module(auth, [
    fluxo_cadastrar/0, fluxo_login/0, fluxo_listar_cadastros/0,
    fluxo_apagar_cadastro_publico/0, apagar_usuario/3
]).

:- use_module(database).
:- use_module(utils).
:- use_module(paciente).
:- use_module(medico).
:- use_module(admin).
:- use_module(persistence).

fluxo_cadastrar :-
    nl,
    writeln('Você deseja se cadastrar como:'),
    writeln('1 - Paciente'),
    writeln('2 - Médico'),
    read_option(Opcao),
    (   Opcao == 1 -> cadastrar_paciente
    ;   Opcao == 2 -> cadastrar_medico
    ;   writeln('Opção inválida.')
    ).

cadastrar_paciente :-
    nl, writeln('Cadastro de Paciente'),
    write('Nome: '), read_string(Nome),
    ler_cpf_unico(CPF),
    ler_username_unico(Username),
    write('Senha: '), read_string(Senha),
    gerar_novo_id(paciente, Id),
    assertz(paciente(Id, Nome, CPF, Username, Senha)),
    salvar_pacientes,
    format('Usuário ~w cadastrado com sucesso!~n', [Username]).

cadastrar_medico :-
    nl, writeln('Cadastro de Médico'),
    write('Nome: '), read_string(Nome),
    ler_crm_unico(CRM),
    write('Especialidade: '), read_string(Especialidade),
    ler_username_unico(Username),
    write('Senha: '), read_string(Senha),
    gerar_novo_id(medico, Id),
    assertz(medico(Id, Nome, CRM, Especialidade, Username, Senha)),
    salvar_medicos,
    format('Usuário ~w cadastrado com sucesso!~n', [Username]).


fluxo_login :-
    nl, write('Digite seu nome de usuário: '), read_string(Username),
    (   get_user_credentials(Username, IdUsuario, Papel, StoredPassword)
    ->  (   verificar_senha(StoredPassword, 3)
        ->  login_success(IdUsuario, Papel, Username)
        ;   writeln('Login falhou. Número máximo de tentativas de senha atingido.')
        )
    ;   writeln('Usuário não encontrado. Faça o cadastro primeiro.')
    ).

get_user_credentials(Username, 0, admin, StoredPassword) :-
    admin(Username, StoredPassword), !.
get_user_credentials(Username, IdUsuario, paciente, StoredPassword) :-
    paciente(IdUsuario, _, _, Username, StoredPassword), !.
get_user_credentials(Username, IdUsuario, medico, StoredPassword) :-
    medico(IdUsuario, _, _, _, Username, StoredPassword).

login_success(IdUsuario, Papel, Username) :-
    (   Papel == admin -> writeln('Login bem-sucedido como Administrador!'), menu_admin
    ;   format('Login bem-sucedido como ~w~n', [Username]),
        (   Papel == paciente -> menu_paciente(IdUsuario, Username)
        ;   Papel == medico -> menu_medico(IdUsuario, Username)
        )
    ).


fluxo_listar_cadastros :-
    nl, writeln('--- Listar Cadastros ---'),
    writeln('Escolha o tipo de cadastro a ser listado:'),
    writeln('1 - Pacientes'),
    writeln('2 - Médicos'),
    read_option(Opcao),
    (   Opcao == 1 -> listar_pacientes
    ;   Opcao == 2 -> listar_medicos
    ;   writeln('Opção inválida.')
    ).

listar_pacientes :-
    findall(Nome-Username, paciente(_, Nome, _, Username, _), Pacientes),
    (   Pacientes = []
    ->  nl, writeln('Nenhum paciente cadastrado.')
    ;   nl, writeln('--- Lista de Pacientes ---'),
        imprimir_lista_pacientes(Pacientes)
    ).

listar_medicos :-
    findall(Nome-Especialidade-Username, medico(_, Nome, _, Especialidade, Username, _), Medicos),
    (   Medicos = []
    ->  nl, writeln('Nenhum médico cadastrado.')
    ;   nl, writeln('--- Lista de Médicos ---'),
        imprimir_lista_medicos(Medicos)
    ).

fluxo_apagar_cadastro_publico :-
    nl, writeln('--- Apagar um Cadastro ---'),
    write('Digite o nome de usuário do cadastro que deseja apagar: '), read_string(Username),
    (   get_user_by_username(Username, Id, Nome, Papel, StoredPassword)
    ->  (   write('Digite a senha para confirmar a exclusão: '),
            verificar_senha(StoredPassword, 3)
        ->  apagar_usuario(Id, Papel, Nome),
            format('Cadastro de \'~w\' apagado com sucesso.~n', [Nome])
        ;   writeln('Exclusão cancelada. Número máximo de tentativas de senha atingido.')
        )
    ;   format('Erro: Usuário \'~w\' não encontrado.~n', [Username])
    ).

apagar_usuario(Id, Papel, _) :-
    (   Papel == paciente
    ->  retract(paciente(Id, _, _, _, _)),
        retractall(mensagem(_, Id, paciente, _, _, _)),
        retractall(mensagem(_, _, _, Id, paciente, _)),
        retractall(ultimo_resultado_triagem(Id, _)),
        salvar_pacientes, salvar_mensagens, salvar_triagens
    ;   Papel == medico
    ->  retract(medico(Id, _, _, _, _, _)),
        retractall(mensagem(_, Id, medico, _, _, _)),
        retractall(mensagem(_, _, _, Id, medico, _)),
        salvar_medicos, salvar_mensagens
    ).
