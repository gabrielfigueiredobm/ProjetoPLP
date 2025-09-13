:- module(sistema, [iniciar/0]).

:- use_module(database).
:- use_module(utils).
:- use_module(auth).
:- use_module(admin).
:- use_module(persistence).

iniciar :-
    setup_database, 
    carregar_dados, 
    (   \+ contador_id(_,_)
    ->  assertz(contador_id(paciente, 0)),
        assertz(contador_id(medico, 0)),
        assertz(contador_id(mensagem, 0))
    ;   true
    ),
    writeln('Bem-vindo ao nosso sistema de telemedicina!'),
    main_loop.

main_loop :-
    nl,
    writeln('Escolha uma opção:'),
    writeln('1 - Cadastrar novo usuário'),
    writeln('2 - Fazer login'),
    writeln('3 - Listar cadastros'),
    writeln('4 - Apagar um cadastro'),
    writeln('5 - Sair'),
    read_option(Opcao),
    processar_main_opcao(Opcao).

processar_main_opcao(1) :- fluxo_cadastrar, main_loop.
processar_main_opcao(2) :- fluxo_login, main_loop.
processar_main_opcao(3) :- fluxo_listar_cadastros, main_loop.
processar_main_opcao(4) :- fluxo_apagar_cadastro_publico, main_loop.
processar_main_opcao(5) :- writeln('Saindo do sistema. Até logo!').
processar_main_opcao(_) :- writeln('Opção inválida. Tente novamente.'), main_loop.
