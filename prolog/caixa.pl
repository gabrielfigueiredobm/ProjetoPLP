:- module(caixa, [
    enviar_mensagem/5,
    ver_mensagens/1,
    apagar_mensagem/3
]).

:- use_module(usuarios, mensagem).

enviar_mensagem(Remetente, Destinatario, Texto, Caixa, NovaCaixa) :-
    append(Caixa, [mensagem(Remetente, Destinatario, Texto)], NovaCaixa).

ver_mensagens(Caixa) :-
    ver_mensagens(Caixa, 1).

ver_mensagens([], _).
ver_mensagens([mensagem(R, D, T)|Tail], I) :-
    format("[~w] De: ~w Para: ~w Texto: ~w~n", [I, R, D, T]),
    I2 is I + 1,
    ver_mensagens(Tail, I2).

apagar_mensagem(Caixa, I, NovaCaixa) :-
    I > 0,
    nth1(I, Caixa, _, NovaCaixa), !.
apagar_mensagem(Caixa, _, Caixa). 