:- module(triagem, [
  doenca/2,
  triagem/2,
  saida/1,
  especialista/2
]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(string)).

doenca("Gripe", ["Febre alta", "Tosse", "Dor no corpo", "Cansaco", "Fraqueza"]).
doenca("Covid", ["Febre", "Tosse", "Perda do olfato", "Falta de ar", "Dor no corpo"]).
doenca("Herpes simples", ["Bolhas na boca (HSV-1)", "Bolhas na regiao genital (HSV-2)", "Coceira", "Febre leve", "Mal-estar"]).
doenca("Hepatite B", ["Ictericia(pele e olhos amarelados)", "Urina escura", "Cansaco", "Nauseas e vomito", "Dor abdominal"]).
doenca("Hepatite C", ["Cansaco", "Dores nas articulacoes", "Urina escura", "Ictericia(pele e olhos amarelados)", "Nauseas e vomito"]).
doenca("Dengue", ["Febre alta", "Dores no corpo", "Manchas vermelhas", "Dor intensa atras dos olhos", "Sangramentos leves(gengiva/nariz)"]).
doenca("Zika", ["Febre baixa", "Manchas vermelhas", "Coceira", "Conjuntivite sem pus", "Dores nas articulacoes"]).
doenca("Chikungunya", ["Febre", "Dores nas articulacoes", "Inchaco nas articulacoes", "Manchas vermelhas", "Dor de cabeca"]).
doenca("Sarampo", ["Febre alta", "Tosse", "Manchas vermelhas", "Conjuntivite", "Coriza"]).
doenca("Varicela", ["Bolhas na pele", "Coceira", "Febre", "Mal-estar", "Perda de apetite"]).
doenca("Pneumonia bacteriana", ["Febre alta", "Dor no peito", "Tosse com catarro", "Dificuldade para respirar", "Calafrios"]).
doenca("Tuberculose", ["Tosse prolongada", "Febre", "Sudorese noturna", "Perda de peso", "Cansaco"]).
doenca("Infeccao urinaria (ITU)", ["Ardencia ao urinar", "Urgencia urinaria", "Dor pelvica", "Febre leve", "Urina turva ou com odor forte"]).
doenca("Gonorreia", ["Corrimento na genitalia", "Dor ao urinar", "Dor pelvica", "Coceira anal ou genital", "Sangramento fora do ciclo(mulheres)"]).
doenca("Sifilis", ["Feridas indolores", "Manchas na pele(maos e pes)", "Febre", "Inguas(linfonodos aumentados)", "Fadiga"]).
doenca("Meningite bacteriana", ["Febre alta", "Rigidez na nuca", "Vomito", "Confusao mental", "Dor de cabeca"]).
doenca("Febre tifoide", ["Febre alta", "Dor abdominal", "Diarreia ou prisao de ventre", "Dor de cabeca", "Mal-estar"]).
doenca("Tetano", ["Espasmos musculares", "Rigidez muscular(mandibula travada)", "Dificuldade para engolir", "Sudorese intensa", "Febre"]).
doenca("Candidiase", ["Coceira genital", "Corrimento genital", "Placas brancas na boca(candidiase oral)", "Ardencia ao urinar", "Dor ao ter relacoes sexuais"]).
doenca("Micose de pele", ["Coceira", "Manchas brancas ou vermelhas", "Descamacao", "Rachaduras na pele", "Ardencia"]).

especialista("Gripe", "Clínico Geral").
especialista("Covid", "Clínico Geral, Infectologista").
especialista("Herpes simples", "Dermatologista").
especialista("Hepatite B", "Hepatologista, Infectologista").
especialista("Hepatite C", "Hepatologista, Infectologista").
especialista("Dengue", "Clínico Geral, Infectologista").
especialista("Zika", "Clínico Geral, Infectologista").
especialista("Chikungunya", "Clínico Geral, Reumatologista").
especialista("Sarampo", "Pediatra, Infectologista").
especialista("Varicela", "Dermatologista").
especialista("Pneumonia bacteriana", "Pneumologista").
especialista("Tuberculose", "Pneumologista, Infectologista").
especialista("Infeccao urinaria (ITU)", "Urologista, Ginecologista").
especialista("Gonorreia", "Infectologista, Urologista, Ginecologista").
especialista("Sifilis", "Infectologista, Dermatologista").
especialista("Meningite bacteriana", "Neurologista, Infectologista").
especialista("Febre tifoide", "Gastroenterologista, Infectologista").
especialista("Tetano", "Infectologista, Neurologista").
especialista("Candidiase", "Ginecologista, Dermatologista").
especialista("Micose de pele", "Dermatologista").

saida(Sintomas) :-
  triagem(Sintomas, TriagemResultado),
  maplist(formataSaida,TriagemResultado).

formataSaida([Doenca, Qnt, Pct]) :-
  especialista_ou_indefinido(Doenca, Esp),
  sufixo(Qnt, Suf),
  format("~w - ~w sintoma~w - ~w% -> Consultar: ~w~n", [Doenca, Qnt, Suf, Pct, Esp]).

especialista_ou_indefinido(Doenca, Esp) :-
    especialista(Doenca, Esp), !.
especialista_ou_indefinido(_, "Especialista Indefinido").

sufixo(1, "").
sufixo(_, "s").

triagem(Sintomas, Resultado) :-
    filtro(Sintomas, Filtrado),
    ordena(Filtrado, Resultado).

filtro(SintomasDoPaciente, Resultado) :-
    limpa_lista_sintomas(SintomasDoPaciente, SintomasPacienteLimpos),
    findall((Doenca, Qnt, Percentual),
        (
            doenca(Doenca, SintomasDoenca),
            limpa_lista_sintomas(SintomasDoenca, SintomasDoencaLimpos),
            include({SintomasPacienteLimpos}/[S]>>memberchk(S, SintomasPacienteLimpos),
                    SintomasDoencaLimpos, SintomasComuns),
            length(SintomasComuns, Qnt),
            length(SintomasDoencaLimpos, Total),
            Qnt > 0,
            percentual(Qnt, Total, Percentual)
        ),
        Resultado).

limpa_sintoma(S, Limpo) :-
    string_lower(S, S1),
    atom_chars(S1, Chars),
    exclude(=(' '), Chars, CharsSemEspaco),
    atom_chars(Limpo, CharsSemEspaco).

limpa_lista_sintomas([], []).
limpa_lista_sintomas([X|Xs], [Y|Ys]) :-
    limpa_sintoma(X, Y),
    limpa_lista_sintomas(Xs, Ys).

ordena(Lista, Ordenada) :-
    map_list_to_pairs(extrai_percentual, Lista, Pares),
    keysort(Pares, ParesOrdenadosAsc),
    reverse(ParesOrdenadosAsc, ParesOrdenados),
    pairs_values(ParesOrdenados, Ordenada).

extrai_percentual((_,_,P), P).

percentual(Qnt, Total, P) :-
    P is (Qnt * 100) // Total.