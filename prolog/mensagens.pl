:- module(mensagens, [
    menu_mensagens_paciente/1, enviar_mensagem/5,
    fluxo_apagar_mensagem/2, imprimir_mensagens_simples/2,
    imprimir_mensagens_detalhadas/2
]).

:- use_module(database).
:- use_module(utils).
:- use_module(persistence).

menu_mensagens_paciente(IdPaciente) :-
    nl, writeln('--- Gerenciar Mensagens ---'),
    writeln('Escolha uma opção:'),
    writeln('1 - Ver caixa de mensagens'),
    writeln('2 - Responder a uma mensagem (de um médico)'),
    writeln('3 - Enviar uma nova mensagem (para um médico)'),
    writeln('4 - Apagar uma mensagem'),
    writeln('5 - Voltar'),
    read_option(Opcao),
    processar_msg_paciente_opcao(Opcao, IdPaciente).

processar_msg_paciente_opcao(1, Id) :- fluxo_ver_mensagens_paciente(Id), menu_mensagens_paciente(Id).
processar_msg_paciente_opcao(2, Id) :- fluxo_responder_mensagem_paciente(Id), menu_mensagens_paciente(Id).
processar_msg_paciente_opcao(3, Id) :- fluxo_enviar_nova_mensagem_paciente(Id), menu_mensagens_paciente(Id).
processar_msg_paciente_opcao(4, Id) :- fluxo_apagar_mensagem(Id, paciente), menu_mensagens_paciente(Id).
processar_msg_paciente_opcao(5, _) :- nl.
processar_msg_paciente_opcao(_, Id) :- writeln('Opção inválida.'), menu_mensagens_paciente(Id).

fluxo_ver_mensagens_paciente(IdPaciente) :-
    findall(IdMsg-Conteudo, mensagem(IdMsg, _, _, IdPaciente, paciente, Conteudo), Mensagens),
    nl, writeln('Caixa de mensagens recebidas:'),
    (   Mensagens = []
    ->  writeln('Nenhuma mensagem na caixa de entrada.')
    ;   imprimir_mensagens_simples(Mensagens, 1)
    ).

fluxo_responder_mensagem_paciente(IdPaciente) :-
    findall(IdMsg-IdRemetente-medico-Conteudo, mensagem(IdMsg, IdRemetente, medico, IdPaciente, paciente, Conteudo), Mensagens),
    (   Mensagens = []
    ->  nl, writeln('Você não tem mensagens para responder.')
    ;   nl, writeln('Escolha a mensagem que deseja responder (digite o número):'),
        imprimir_mensagens_detalhadas(Mensagens, 1),
        read_option(Escolha),
        (   nth1(Escolha, Mensagens, Selecionada)
        ->  Selecionada = IdMsg-IdMedico-_-_,
            medico(IdMedico, NomeMedico, _, _, _, _),
            format('Digite o texto da mensagem para ~w:~n', [NomeMedico]),
            read_string(Resposta),
            enviar_mensagem(IdPaciente, paciente, IdMedico, medico, Resposta),
            format('Mensagem enviada para ~w: ~w~n', [NomeMedico, Resposta])
        ;   writeln('Seleção inválida.')
        )
    ).

fluxo_enviar_nova_mensagem_paciente(IdPaciente) :-
    findall(Id-Nome-Especialidade, medico(Id, Nome, _, Especialidade, _, _), Medicos),
    (   Medicos = []
    ->  nl, writeln('Nenhum médico disponível no momento.')
    ;   nl, writeln('Escolha o destinatario médico digitando o número correspondente:'),
        imprimir_lista_medicos_selecao(Medicos, 1),
        read_option(Escolha),
        (   nth1(Escolha, Medicos, Selecionado)
        ->  Selecionado = IdMedico-NomeMedico-_,
            format('Digite o texto da mensagem para ~w:~n', [NomeMedico]),
            read_string(Conteudo),
            enviar_mensagem(IdPaciente, paciente, IdMedico, medico, Conteudo),
            format('Mensagem enviada para ~w: ~w~n', [NomeMedico, Conteudo])
        ;   writeln('Seleção inválida.')
        )
    ).

fluxo_apagar_mensagem(IdUsuario, Papel) :-
    findall(IdMsg-IdRemetente-PapelRemetente-Conteudo, mensagem(IdMsg, IdRemetente, PapelRemetente, IdUsuario, Papel, Conteudo), Mensagens),
    (   Mensagens = []
    ->  nl, writeln('Sua caixa de mensagens está vazia.')
    ;   nl, writeln('Escolha a mensagem que deseja apagar (digite o número):'),
        imprimir_mensagens_detalhadas(Mensagens, 1),
        write('Número da mensagem: '), read_option(Escolha),
        (   nth1(Escolha, Mensagens, Selecionada)
        ->  Selecionada = IdMsg-_-_-_,
            retract(mensagem(IdMsg, _, _, _, _, _)),
            salvar_mensagens,
            writeln('Mensagem apagada com sucesso!')
        ;   writeln('Seleção inválida.')
        )
    ).

enviar_mensagem(IdRemetente, PapelRemetente, IdDestinatario, PapelDestinatario, Conteudo) :-
    gerar_novo_id(mensagem, IdMsg),
    assertz(mensagem(IdMsg, IdRemetente, PapelRemetente, IdDestinatario, PapelDestinatario, Conteudo)),
    salvar_mensagens.

imprimir_mensagens_simples([], _).
imprimir_mensagens_simples([_-Conteudo | Resto], N) :-
    format('[~w]~w~n', [N, Conteudo]),
    N1 is N + 1,
    imprimir_mensagens_simples(Resto, N1).

imprimir_mensagens_detalhadas([], _).
imprimir_mensagens_detalhadas([_-IdRemetente-PapelRemetente-Conteudo | Resto], N) :-
    (   PapelRemetente == paciente -> paciente(IdRemetente, Nome, _, _, _)
    ;   PapelRemetente == medico -> medico(IdRemetente, Nome, _, _, _, _)
    ),
    format('~w - De: ~w, Texto: ~w~n', [N, Nome, Conteudo]),
    N1 is N + 1,
    imprimir_mensagens_detalhadas(Resto, N1).
