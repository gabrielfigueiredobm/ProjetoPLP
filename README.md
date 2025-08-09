# PROJETO DA DISCIPLINA DE PARADIGMAS DE LINGUAGENS DE PROGRAMAÇÃO - SISTEMA DE TELEMEDICINA

## Participantes: Gabriel Figueiredo, Pedro Inocêncio e Kaiky Magno

### Resumo do sistema

### Objetivo
   Desenvolver um sistema de telemedicina, capaz de auxiliar o paciente a diagnosticar infecções virais, bacterianas e fúngicas, utilizando como entrada os sintomas digitados pelo paciente que posterior ao envio passarão pela triagem para tratamento desses dados e logo após, envio para o médico responsável pelo atendimento de tal paciente.

### Fluxo geral
#### Login
   O usuário informa primeiramente se vai logar num perfil de médico ou de paciente.

#### Entrada de dados
   O paciente informa todos os seus sintomas manualmente no sistema (por exemplo: febre, dor de cabeça, cansaço etc.).


#### Filtragem inicial
   O sistema recebe os sintomas e faz uma filtragem (ex.: remove repetições, padroniza nomes, ignora informações irrelevantes) e passa para a classe de triagem, responsável pelo cálculo das probabilidades e pela documentação geral da triagem para posterior chegada até o médico responsável.


#### Processamento e diagnóstico
O sistema compara os sintomas com o armazenamento de doenças que contém:

* Lista de doenças (virais, bacterianas, fúngicas)

* Sintomas típicos de cada doença

* Probabilidades associadas


   Ele calcula a chance (probabilidade) de o paciente ter cada uma das doenças cadastradas, quanto mais o paciente se aproximar do total de sintomas de uma doença, maior será a probabilidade do paciente estar com tal doença.


#### Auxílio ao médico responsável
O sistema mostra ao médico os percentuais calculados.

A partir disso, o médico vai saber que tratamento sugerir.


#### Caixa de Mensagem
   Após o diagnóstico da possível doença por parte do sistema e o encaminhamento ao médico especialista, será criada uma caixa de mensagem para os pacientes e médicos, como uma espécie de e-mail, onde eles poderão se comunicar entre si. 
#Armazenamento 
Todas as doenças, sintomas e medidas preventivas ficam armazenados em uma estrutura de dados.


### Possível estrutura técnica
#### Classes ou módulos principais

* Usuários: Médico ou Paciente

* Triagem: Responsável pelo processamento da entrada (sintomas) informada pelo paciente e enviará esses dados processados para o médico responsável pelo atendimento.

* Doença: contém nome, tipo (viral, bacteriana, fúngica), lista de sintomas, medidas profiláticas.


### Funcionalidades principais

* Entrada e armazenamento dos sintomas.

* Processamento dos sintomas para calcular a probabilidade do paciente ter as doenças cadastradas no sistema, possibilitando ao médico responsável ter maior assertividade no diagnóstico e sugestão de tratamento.

* A partir do processamento e do envio dos dados ao médico responsável, se inicia a conversa entre paciente e médico.

* Por fim, espera-se que o médico sugira algum tratamento para o paciente e o atendimento se encerra.

### Instalação

* Certificar-se que tem o GHC instalado
```
ghc --version
```
* Entrar no repositório correto
```
cd haskell
```
* Compilar o programa
```
ghc main
```
* Rodar o programa
```
./main.exe
```
