:- module(admin, [menu_admin/0]).

:- use_module(database).
:- use_module(utils).
:- use_module(auth, [apagar_usuario/3]).
:- use_module(persistence).

menu_admin :-
    nl, writeln('--- MENU DO ADMINISTRADOR ---'),
    writeln('Escolha uma opção:'),
    writeln('1 - Listar todos os usuários'),
    writeln('2 - Apagar qualquer cadastro (sem senha)'),
    writeln('3 - Mudar senha de um usuário'),
    writeln('4 - Sair'),
    read_option(Opcao),
    processar_admin_opcao(Opcao).

processar_admin_opcao(1) :- fluxo_listar_adm, menu_admin.
processar_admin_opcao(2) :- fluxo_apagar_adm, menu_admin.
processar_admin_opcao(3) :- fluxo_mudar_senha_adm, menu_admin.
processar_admin_opcao(4) :- writeln('Saindo do menu do administrador...').
processar_admin_opcao(_) :- writeln('Opção inválida.'), menu_admin.

fluxo_listar_adm :-
    nl, writeln('--- LISTA COMPLETA DE USUÁRIOS ---'),
    findall(Nome-Username, (paciente(_, Nome, _, Username, _); medico(_, Nome, _, _, Username, _)), Usuarios),
    (   Usuarios = []
    ->  writeln('Nenhum usuário cadastrado no sistema.')
    ;   imprimir_lista_adm(Usuarios)
    ).

fluxo_apagar_adm :-
    nl, writeln('--- Apagar um Cadastro ---'),
    write('Digite o nome de usuário do cadastro que deseja apagar: '), read_string(Username),
    (   get_user_by_username(Username, Id, Nome, Papel, _)
    ->  apagar_usuario(Id, Papel, Nome),
        format('Cadastro de \'~w\' apagado com sucesso.~n', [Nome])
    ;   format('Erro: Usuário \'~w\' não encontrado.~n', [Username])
    ).

fluxo_mudar_senha_adm :-
    nl, writeln('--- Mudar Senha do Usuário ---'),
    write('Digite o nome de usuário do cadastro que deseja mudar a senha: '), read_string(Username),
    (   paciente(Id, Nome, Cpf, Username, _)
    ->  write('Digite a nova senha: '), read_string(NovaSenha),
        retract(paciente(Id, Nome, Cpf, Username, _)),
        assertz(paciente(Id, Nome, Cpf, Username, NovaSenha)),
        salvar_pacientes,
        format('A senha do usuário \'~w\' foi alterada com sucesso!~n', [Username])
    ;   medico(Id, Nome, Crm, Esp, Username, _)
    ->  write('Digite a nova senha: '), read_string(NovaSenha),
        retract(medico(Id, Nome, Crm, Esp, Username, _)),
        assertz(medico(Id, Nome, Crm, Esp, Username, NovaSenha)),
        salvar_medicos,
        format('A senha do usuário \'~w\' foi alterada com sucesso!~n', [Username])
    ;   format('Erro: Usuário \'~w\' não encontrado.~n', [Username])
    ).
