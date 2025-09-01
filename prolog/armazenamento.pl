:- module(armazenamento, [
    salvar_usuarios/1,
    carregar_usuarios/1,
    salvar_mensagens/1,
    carregar_mensagens/1
]).

salvar_usuarios(Usuarios) :-
    open('database/usuarios.txt', write, Stream),
    forall(member(U, Usuarios),
           (writeq(Stream, U), write(Stream, '.'), nl(Stream))),
    close(Stream).

carregar_usuarios(Usuarios) :-
    exists_file('database/usuarios.txt'),
    !,
    open('database/usuarios.txt', read, Stream),
    carregar_usuarios_stream(Stream, Usuarios),
    close(Stream).

carregar_usuarios([]).

carregar_usuarios_stream(Stream, []) :-
    at_end_of_stream(Stream).

carregar_usuarios_stream(Stream, [U|Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, U),
    carregar_usuarios_stream(Stream, Rest).

salvar_mensagens(Mensagens) :-
    open('database/mensagens.txt', write, Stream),
    forall(member(M, Mensagens),
           (writeq(Stream, M), write(Stream, '.'), nl(Stream))),
    close(Stream).

carregar_mensagens(Mensagens) :-
    exists_file('database/mensagens.txt'),
    !,
    open('database/mensagens.txt', read, Stream),
    carregar_mensagens_stream(Stream, Mensagens),
    close(Stream).

carregar_mensagens([]).

carregar_mensagens_stream(Stream, []) :-
    at_end_of_stream(Stream).

carregar_mensagens_stream(Stream, [M|Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, M),
    carregar_mensagens_stream(Stream, Rest).