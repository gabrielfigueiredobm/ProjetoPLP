:- module(database, [
    paciente/5, medico/6, contador_id/2, mensagem/6, ultimo_resultado_triagem/2,
    doenca/3, admin/2, setup_database/0
]).

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

:- dynamic paciente/5, medico/6, contador_id/2, mensagem/6, ultimo_resultado_triagem/2.

setup_database :-
    retractall(paciente(_,_,_,_,_)),
    retractall(medico(_,_,_,_,_,_)),
    retractall(contador_id(_,_)),
    retractall(mensagem(_,_,_,_,_,_)),
    retractall(ultimo_resultado_triagem(_,_)).

admin("Adm", "PIrs1234@").

doenca('Gripe', ['Febre alta', 'Tosse', 'Dor no corpo', 'Cansaço', 'Fraqueza'], ['Clínico Geral']).
doenca('Covid', ['Febre', 'Tosse', 'Perda do olfato', 'Falta de ar', 'Dor no corpo'], ['Clínico Geral', 'Infectologista']).
doenca('Herpes simples', ['Bolhas na boca (HSV-1)', 'Bolhas na regiao genital (HSV-2)', 'Coceira', 'Febre leve', 'Mal-estar'], ['Dermatologista']).
doenca('Hepatite B', ['Ictericia(pele e olhos amarelados', 'Urina escura', 'Cansaço', 'Nauseas e vomito', 'Dor abdominal'], ['Hepatologista', 'Infectologista']).
doenca('Hepatite C', ['Cansaço', 'Dores nas articulacoes', 'Urina escura', 'Ictericia(pele e olhos amarelados)', 'Nauseas e vomito'], ['Hepatologista', 'Infectologista']).
doenca('Dengue', ['Febre alta', 'Dores no corpo', 'Manchas vermelhas', 'Dor intensa atras dos olhos', 'Sangramentos leves(gengiva/nariz)'], ['Clínico Geral', 'Infectologista']).
doenca('Zika', ['Febre baixa', 'Manchas vermelhas', 'Coceira', 'Conjuntivite sem pus', 'Dores nas articulacoes'], ['Clínico Geral', 'Infectologista']).
doenca('Chikungunya', ['Febre', 'Dores nas articulacoes', 'Inchaco nas articulacoes', 'Manchas vermelhas', 'Dor de cabeca'], ['Clínico Geral', 'Reumatologista']).
doenca('Sarampo', ['Febre alta', 'Tosse', 'Manchas vermelhas', 'Conjuntivite', 'Coriza'], ['Pediatra', 'Infectologista']).
doenca('Varicela', ['Bolhas na pele', 'Coceira', 'Febre', 'Mal-estar', 'Perda de apetite'], ['Dermatologista']).
doenca('Pneumonia bacteriana', ['Febre alta', 'Dor no peito', 'Tosse com catarro', 'Dificuldade para respirar', 'Calafrios'], ['Pneumologista']).
doenca('Tuberculose', ['Tosse prolongada', 'Febre', 'Sudorese noturna', 'Perda de peso', 'Cansaço'], ['Pneumologista', 'Infectologista']).
doenca('Infeccao urinaria (ITU)', ['Ardencia ao urinar', 'Urgencia urinaria', 'Dor pelvica', 'Febre leve', 'Urina turva ou com odor forte'], ['Urologista', 'Ginecologista']).
doenca('Gonorreia', ['Corrimento na genitalia', 'Dor ao urinar', 'Dor pelvica', 'Coceira anal ou genital', 'Sangramento fora do ciclo(mulheres)'], ['Infectologista', 'Urologista', 'Ginecologista']).
doenca('Sifilis', ['Feridas indolores', 'Manchas na pele(maos e pes)', 'Febre', 'Inguas(linfonodos aumentados)', 'Fadiga'], ['Infectologista', 'Dermatologista']).
doenca('Meningite bacteriana', ['Febre alta', 'Rigidez na nuca', 'Vomito', 'Confusao mental', 'Dor de cabeca'], ['Neurologista', 'Infectologista']).
doenca('Febre tifoide', ['Febre alta', 'Dor abdominal', 'Diarreia ou prisao de ventre', 'Dor de cabeca', 'Mal-estar'], ['Gastroenterologista', 'Infectologista']).
doenca('Tetano', ['Espasmos musculares', 'Rigidez muscular(mandibula travada)', 'Dificuldade para engolir', 'Sudorese intensa', 'Febre'], ['Infectologista', 'Neurologista']).
doenca('Candidiase', ['Coceira genital', 'Corrimento genital', 'Placas brancas na boca(candidiase oral)', 'Ardencia ao urinar', 'Dor ao ter relacoes sexuais'], ['Ginecologista', 'Dermatologista']).
doenca('Micose de pele', ['Coceira', 'Manchas brancas ou vermelhas', 'Descamacao', 'Rachaduras na pele', 'Ardencia'], ['Dermatologista']).
