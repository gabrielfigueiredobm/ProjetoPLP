:- module(usuarios, [cria_paciente/3, cria_medico/4, cria_admin/2, get_nome/2, get_cpf/2, get_crm/2, get_senha/2, get_especialidade/2, get_mensagens/2]).

:- use_module(mensagem).

cria_paciente(+Nome, +Cpf, +Senha, -Paciente) :-
    Paciente = paciente{
        nome: Nome,
        cpf: Cpf,
        senha: Senha,
        mensagens: [],
        ultimas_triagens: []
    }.

cria_medico(+Nome, +Crm, +Senha, +Especialidade, -Medico) :-
    Medico = medico{
        nome: Nome,
        crm: Crm,
        senha: Senha,
        especialidade: Especialidade,
        mensagens: [],
        triagens: []
    }.

cria_admin(+Username, +Senha, -Admin):-
    Admin = admin{
        username: Username,
        senha: Senha
    }.

get_nome(Usuario, Nome) :-
    Nome = Usuario.nome.

get_senha(Usuario, Senha) :-
    Senha = Usuario.senha.

get_mensagens(Usuario, Mensagens):-
    Mensagens = Usuario.mensagens.

get_cpf(Usuario, Cpf) :-
    Cpf = Usuario.cpf.

get_crm(Usuario, Crm) :-
    Crm = Usuario.crm.

get_especialidade(Usuario, Especialidade) :-
    Especialidade = Usuario.especialidade.
