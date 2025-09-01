:- module(mensagem, []).

cria_mensagem(+Remetente, +Destinatario, +Texto, -Mensagem) :-
    Mensagem = mensagem{
        remetente: Remetente,
        destinatario: Destinatario,
        texto: Texto
    }.