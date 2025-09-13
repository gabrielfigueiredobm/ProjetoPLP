:- module(utils, [
    read_string/1, read_option/1,
    ler_cpf_unico/1, ler_crm_unico/1, ler_username_unico/1,
    verificar_senha/2, gerar_novo_id/2,
    imprimir_lista_simples/1, imprimir_lista_pacientes/1,
    imprimir_lista_medicos/1, imprimir_lista_adm/1,
    imprimir_lista_medicos_selecao/2, normalizar_sintoma/2,
    get_user_by_username/5
]).

:- use_module(database).
:- use_module(persistence).


read_string(String) :- read_line_to_string(user_input, String).
read_option(Number) :-
    read_line_to_string(user_input, String),
    ( atom_number(String, Number) -> true ; Number = -1 ).


ler_cpf_unico(CPF) :-
    write('CPF: '), read_string(InputCPF),
    (   paciente(_, _, InputCPF, _, _)
    ->  writeln('Este CPF já está cadastrado. Por favor, informe outro.'),
        ler_cpf_unico(CPF)
    ;   CPF = InputCPF
    ).

ler_crm_unico(CRM) :-
    write('CRM: '), read_string(InputCRM),
    (   medico(_, _, InputCRM, _, _, _)
    ->  writeln('Este CRM já está cadastrado. Por favor, informe outro.'),
        ler_crm_unico(CRM)
    ;   CRM = InputCRM
    ).

ler_username_unico(Username) :-
    write('Username: '), read_string(InputUsername),
    (   (paciente(_, _, _, InputUsername, _); medico(_, _, _, _, InputUsername, _); admin(InputUsername, _))
    ->  writeln('Este Username já está em uso. Por favor, escolha outro.'),
        ler_username_unico(Username)
    ;   Username = InputUsername
    ).


verificar_senha(SenhaCorreta, Tentativas) :-
    Tentativas > 0,
    write('Senha: '), read_string(InputSenha),
    (   InputSenha == SenhaCorreta
    ->  true
    ;   NovaTentativa is Tentativas - 1,
        ( NovaTentativa > 0 -> writeln('Senha incorreta. Tente novamente.') ; true ),
        verificar_senha(SenhaCorreta, NovaTentativa)
    ).

get_user_by_username(Username, Id, Nome, paciente, StoredPassword) :-
    paciente(Id, Nome, _, Username, StoredPassword), !.
get_user_by_username(Username, Id, Nome, medico, StoredPassword) :-
    medico(Id, Nome, _, _, Username, StoredPassword).


gerar_novo_id(Tipo, NovoId) :-
    retract(contador_id(Tipo, IdAtual)),
    NovoId is IdAtual + 1,
    assertz(contador_id(Tipo, NovoId)),
    salvar_contadores.



imprimir_lista_simples([]).
imprimir_lista_simples([H|T]) :-
    writeln(H),
    imprimir_lista_simples(T).

imprimir_lista_pacientes([]).
imprimir_lista_pacientes([Nome-Username | Resto]) :-
    format('Nome: ~w, Username: ~w~n', [Nome, Username]),
    imprimir_lista_pacientes(Resto).

imprimir_lista_medicos([]).
imprimir_lista_medicos([Nome-Especialidade-Username | Resto]) :-
    format('Dr(a). ~w - Especialidade: ~w, Username: ~w~n', [Nome, Especialidade, Username]),
    imprimir_lista_medicos(Resto).

imprimir_lista_adm([]).
imprimir_lista_adm([Nome-Username | Resto]) :-
    format('~w | Username: ~w~n', [Nome, Username]),
    imprimir_lista_adm(Resto).

imprimir_lista_medicos_selecao([], _).
imprimir_lista_medicos_selecao([_-Nome-Especialidade | Resto], N) :-
    format('~w - ~w - Especialidade: ~w.~n', [N, Nome, Especialidade]),
    N1 is N + 1,
    imprimir_lista_medicos_selecao(Resto, N1).


normalizar_sintoma(Sintoma, Normalizado) :-
    string_lower(Sintoma, Lower),
    string_codes(Lower, Codes),
    exclude(=(32), Codes, CodesSemEspaco),
    string_codes(Normalizado, CodesSemEspaco).
