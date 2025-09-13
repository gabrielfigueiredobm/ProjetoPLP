:- module(persistence, [
    carregar_dados/0,
    salvar_pacientes/0,
    salvar_medicos/0,
    salvar_mensagens/0,
    salvar_triagens/0,
    salvar_contadores/0
]).

:- use_module(database).


salvar_pacientes :- salvar_para_arquivo('pacientes.txt', paciente(_,_,_,_,_)).
salvar_medicos :- salvar_para_arquivo('medicos.txt', medico(_,_,_,_,_,_)).
salvar_mensagens :- salvar_para_arquivo('mensagens.txt', mensagem(_,_,_,_,_,_)).
salvar_triagens :- salvar_para_arquivo('triagens.txt', ultimo_resultado_triagem(_,_)).
salvar_contadores :- salvar_para_arquivo('contadores.txt', contador_id(_,_)).

salvar_para_arquivo(Arquivo, Termo) :-
    open(Arquivo, write, Stream),
    forall(
        call(Termo),
        write_term(Stream, Termo, [quoted(true), fullstop(true), nl(true)])
    ),
    close(Stream).

carregar_dados :-
    carregar_de_arquivo('pacientes.txt'),
    carregar_de_arquivo('medicos.txt'),
    carregar_de_arquivo('mensagens.txt'),
    carregar_de_arquivo('triagens.txt'),
    carregar_de_arquivo('contadores.txt').

carregar_de_arquivo(Arquivo) :-
    exists_file(Arquivo), !,
    open(Arquivo, read, Stream),
    carregar_termos(Stream),
    close(Stream).
carregar_de_arquivo(_).

carregar_termos(Stream) :-
    read_term(Stream, Termo, []),
    (   Termo == end_of_file
    ->  true
    ;   assertz(Termo),
        carregar_termos(Stream)
    ).
