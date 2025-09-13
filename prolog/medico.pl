:- module(medico, [menu_medico/2]).

:- use_module(database).
:- use_module(utils).
:- use_module(mensagens).

menu_medico(IdMedico, NomeMedico) :-
    nl, format('Bem-vindo(a), Dr(a). ~w!~n', [NomeMedico]),
    writeln('Escolha uma opção:'),
    writeln('1 - Ver caixa de mensagens'),
    writeln('2 - Responder a uma mensagem'),
    writeln('3 - Apagar uma mensagem'),
    writeln('4 - Sair'),
    read_option(Opcao),
    processar_medico_opcao(Opcao, IdMedico, NomeMedico).

processar_medico_opcao(1, Id, Nome) :- fluxo_ver_mensagens_medico(Id), menu_medico(Id, Nome).
processar_medico_opcao(2, Id, Nome) :- fluxo_responder_mensagem_medico(Id), menu_medico(Id, Nome).
processar_medico_opcao(3, Id, Nome) :- fluxo_apagar_mensagem(Id, medico), menu_medico(Id, Nome).
processar_medico_opcao(4, _, _) :- writeln('Saindo do menu do médico...').
processar_medico_opcao(_, Id, Nome) :- writeln('Opção inválida.'), menu_medico(Id, Nome).

fluxo_ver_mensagens_medico(IdMedico) :-
    findall(IdMsg-Conteudo, mensagem(IdMsg, _, _, IdMedico, medico, Conteudo), Mensagens),
    nl, writeln('Caixa de mensagens recebidas:'),
    (   Mensagens = []
    ->  writeln('Nenhuma mensagem na caixa de entrada.')
    ;   imprimir_mensagens_simples(Mensagens, 1)
    ).

fluxo_responder_mensagem_medico(IdMedico) :-
    findall(IdMsg-IdRemetente-paciente-Conteudo, mensagem(IdMsg, IdRemetente, paciente, IdMedico, medico, Conteudo), Mensagens),
    (   Mensagens = []
    ->  nl, writeln('Você não tem mensagens para responder.')
    ;   nl, writeln('Escolha a mensagem que deseja responder (digite o número):'),
        imprimir_mensagens_detalhadas(Mensagens, 1),
        read_option(Escolha),
        (   nth1(Escolha, Mensagens, Selecionada)
        ->  Selecionada = IdMsg-IdPaciente-_-_,
            paciente(IdPaciente, NomePaciente, _, _, _),
            nl, writeln('--- Últimos Resultados da Triagem do Paciente ---'),
            (   ultimo_resultado_triagem(IdPaciente, Resultados)
            ->  imprimir_lista_simples(Resultados)
            ;   writeln('Paciente não realizou nenhuma triagem.')
            ),
            writeln('---------------------------------------------'),
            format('Digite o texto da mensagem para ~w:~n', [NomePaciente]),
            read_string(Resposta),
            enviar_mensagem(IdMedico, medico, IdPaciente, paciente, Resposta),
            format('Mensagem enviada para ~w: ~w~n', [NomePaciente, Resposta])
        ;   writeln('Seleção inválida.')
        )
    ).
