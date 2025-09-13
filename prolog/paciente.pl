:- module(paciente, [menu_paciente/2]).

:- use_module(database).
:- use_module(utils).
:- use_module(mensagens).
:- use_module(persistence).

menu_paciente(IdPaciente, NomePaciente) :-
    nl, format('Bem-vindo(a), ~w!~n', [NomePaciente]),
    writeln('Escolha uma opção:'),
    writeln('1 - Fazer triagem (informar sintomas)'),
    writeln('2 - Gerenciar mensagens'),
    writeln('3 - Sair'),
    read_option(Opcao),
    processar_paciente_opcao(Opcao, IdPaciente, NomePaciente).

processar_paciente_opcao(1, Id, Nome) :- fluxo_triagem(Id), menu_paciente(Id, Nome).
processar_paciente_opcao(2, Id, Nome) :- menu_mensagens_paciente(Id), menu_paciente(Id, Nome).
processar_paciente_opcao(3, _, _) :- writeln('Saindo do menu do paciente...').
processar_paciente_opcao(_, Id, Nome) :- writeln('Opção inválida.'), menu_paciente(Id, Nome).

fluxo_triagem(IdPaciente) :-
    nl, writeln('Digite os sintomas separados por vírgula (exemplo: Febre, Tosse):'),
    writeln('Sintomas possíveis:'),
    writeln('. Febre alta, Tosse, Dor no corpo, Cansaço, Fraqueza'),
    writeln('. Febre, Perda do olfato, Falta de ar, Bolhas na boca (HSV-1), Bolhas na regiao genital (HSV-2)'),
    writeln('. Coceira, Febre leve, Mal-estar, Ictericia(pele e olhos amarelados), Urina escura'),
    writeln('. Nauseas e vomito, Dor abdominal, Dores nas articulacoes, Ictericia(pele e olhos amarelados), Dores no corpo'),
    writeln('. Manchas vermelhas, Dor intensa atras dos olhos, Sangramentos leves(gengiva/nariz), Febre baixa, Conjuntivite sem pus'),
    writeln('. Inchaco nas articulacoes, Dor de cabeca, Conjuntivite, Coriza, Bolhas na pele'),
    writeln('. Perda de apetite, Dor no peito, Tosse com catarro, Dificuldade para respirar, Calafrios'),
    writeln('. Tosse prolongada, Sudorese noturna, Perda de peso, Ardencia ao urinar, Urgencia urinaria'),
    writeln('. Dor pelvica, Urina turva ou com odor forte, Corrimento na genitalia, Dor ao urinar, Coceira anal ou genital'),
    writeln('. Sangramento fora do ciclo(mulheres), Feridas indolores, Manchas na pele(maos e pes), Inguas(linfonodos aumentados), Fadiga'),
    writeln('. Rigidez na nuca, Vomito, Confusao mental, Diarreia ou prisao de ventre, Espasmos musculares'),
    writeln('. Rigidez muscular(mandibula travada), Dificuldade para engolir, Sudorese intensa, Coceira genital, Corrimento genital'),
    writeln('. Placas brancas na boca(candidiase oral), Dor ao ter relacoes sexuais, Manchas brancas ou vermelhas, Descamacao, Rachaduras na pele'),
    writeln('. Ardencia'),
    read_string(SintomasInput),
    split_string(SintomasInput, ",", " ", SintomasPaciente),
    maplist(normalizar_sintoma, SintomasPaciente, SintomasNormalizados),
    findall(
        Pct-Qnt-Doenca-Especialistas,
        (   doenca(Doenca, SintomasDoenca, Especialistas),
            maplist(normalizar_sintoma, SintomasDoenca, SintomasDoencaNormalizados),
            intersection(SintomasNormalizados, SintomasDoencaNormalizados, Comum),
            length(Comum, Qnt),
            Qnt > 0,
            length(SintomasDoencaNormalizados, Total),
            Pct is (Qnt * 100) // Total
        ),
        Resultados
    ),
    sort(0, @>=, Resultados, ResultadosOrdenados),
    nl, writeln('Resultados da Triagem:'),
    (   ResultadosOrdenados = []
    ->  writeln('Nenhuma correspondência encontrada com os sintomas informados.')
    ;   maplist(formatar_saida_triagem, ResultadosOrdenados, SaidasFormatadas),
        retractall(ultimo_resultado_triagem(IdPaciente, _)),
        assertz(ultimo_resultado_triagem(IdPaciente, SaidasFormatadas)),
        salvar_triagens,
        imprimir_lista_simples(SaidasFormatadas)
    ).

formatar_saida_triagem(Pct-Qnt-Doenca-Especialistas, SaidaFormatada) :-
    ( Qnt =:= 1 -> Sufixo = "" ; Sufixo = "s" ),
    atomic_list_concat(Especialistas, ', ', EspecialistasStr),
    format(string(SaidaFormatada), '~w - ~w sintoma~w - ~w% -> Consultar um(a) ~w', [Doenca, Qnt, Sufixo, Pct, EspecialistasStr]).
