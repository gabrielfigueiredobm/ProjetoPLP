module BasesDeDados (doencas, triagem) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Char (toLower)

type Doenca = String
type Sintomas = [String]

doencas :: [(Doenca, Sintomas)]
doencas = 
   [ ("Gripe", ["Febre alta", "Tosse", "Dor no corpo", "Cansaco", "Fraqueza"])
   , ("Covid", ["Febre", "Tosse", "Perda do olfato", "Falta de ar", "Dor no corpo"])
   , ("Herpes simples", ["Bolhas na boca (HSV-1)", "Bolhas na regiao genital (HSV-2)", "Coceira", "Febre leve", "Mal-estar"])
   , ("Hepatite B", ["Ictericia(pele e olhos amarelados", "Urina escura", "Cansaco", "Nauseas e vomito", "Dor abdominal"])
   , ("Hepatite C", ["Cansaco", "Dores nas articulacoes", "Urina escura", "Ictericia(pele e olhos amarelados)", "Nauseas e vomito"])
   , ("Dengue", ["Febre alta", "Dores no corpo", "Manchas vermelhas", "Dor intensa atras dos olhos", "Sangramentos leves(gengiva/nariz)"])
   , ("Zika", ["Febre baixa", "Manchas vermelhas", "Coceira", "Conjuntivite sem pus", "Dores nas articulacoes"])
   , ("Chikungunya", ["Febre", "Dores nas articulacoes", "Inchaco nas articulacoes", "Manchas vermelhas", "Dor de cabeca"])
   , ("Sarampo", ["Febre alta", "Tosse", "Manchas vermelhas", "Conjuntivite", "Coriza"])
   , ("Varicela", ["Bolhas na pele", "Coceira", "Febre", "Mal-estar", "Perda de apetite"])
   , ("Pneumonia bacteriana", ["Febre alta", "Dor no peito", "Tosse com catarro", "Dificuldade para respirar", "Calafrios"])
   , ("Tuberculose", ["Tosse prolongada", "Febre", "Sudorese noturna", "Perda de peso", "Cansaco"])
   , ("Infeccao urinaria (ITU)", ["Ardencia ao urinar", "Urgencia urinaria", "Dor pelvica", "Febre leve", "Urina turva ou com odor forte"])
   , ("Gonorreia", ["Corrimento na genitalia", "Dor ao urinar", "Dor pelvica", "Coceira anal ou genital", "Sangramento fora do ciclo(mulheres)"])
   , ("Sifilis", ["Feridas indolores", "Manchas na pele(maos e pes)", "Febre", "Inguas(linfonodos aumentados)", "Fadiga"])
   , ("Meningite bacteriana", ["Febre alta", "Rigidez na nuca", "Vomito", "Confusao mental", "Dor de cabeca"])
   , ("Febre tifoide", ["Febre alta", "Dor abdominal", "Diarreia ou prisao de ventre", "Dor de cabeca", "Mal-estar"])
   , ("Tetano", ["Espasmos musculares", "Rigidez muscular(mandibula travada)", "Dificuldade para engolir", "Sudorese intensa", "Febre"])
   , ("Candidiase", ["Coceira genital", "Corrimento genital", "Placas brancas na boca(candidiase oral)", "Ardencia ao urinar", "Dor ao ter relacoes sexuais"])
   , ("Micose de pele", ["Coceira", "Manchas brancas ou vermelhas", "Descamacao", "Rachaduras na pele", "Ardencia"])
   ]

triagem :: Sintomas -> [(Doenca, Int, Int)]
triagem = ordena . filtro

limpaSintoma :: String -> String
limpaSintoma = map toLower . filter (/= ' ')

limpaListaSintomas :: [String] -> [String]
limpaListaSintomas [] = []
limpaListaSintomas (x:xs) = limpaSintoma x : limpaListaSintomas xs

filtro :: Sintomas -> [(Doenca, Int, Int)]
filtro sintomasDoPaciente = 
   let sintomasPacienteLimpos = limpaListaSintomas sintomasDoPaciente
   in [(doenca, qnt, percentual qnt total) 
   | (doenca, sintomasDoenca) <- doencas
   , let sintomasDoencaLimpos = limpaListaSintomas sintomasDoenca
         qnt = length (filter (`elem` sintomasPacienteLimpos) sintomasDoencaLimpos)
         total = length sintomasDoencaLimpos
   , qnt > 0
   ]

ordena :: [(Doenca, Int, Int)] -> [(Doenca, Int , Int)]
ordena = sortBy (comparing (\(_, _, p) -> Down p))

percentual :: Int -> Int -> Int
percentual qnt total = (qnt * 100) `div` total
