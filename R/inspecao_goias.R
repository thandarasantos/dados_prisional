library(tidyverse)
library(janitor)

# filtra GO dentro da tabela de microdados gerada por microdados_infopen_abertura.R
df_goias <- df_infopen_microdados %>% 
  filter(UF == "GO") 


df <- df_goias %>% 
  clean_names() %>% 
  arrange(nome_do_estabelecimento, ano, semestre) %>% 
  select(ano, 
         semestre, 
         nome_do_estabelecimento, 
         endereco,
         tipo = x1_1_estabelecimento_originalmente_destinado_a_pessoa_privadas_de_liberdade_do_sexo,
         regime = x1_2_tipo_de_estabelecimento_originalmente_destinado,
         capacidade_masc = x1_3_capacidade_do_estabelecimento_masculino_total,
         capacidade_fem = x1_3_capacidade_do_estabelecimento_feminino_total,
         capacidade_provisorios = x1_3_capacidade_do_estabelecimento_presos_provisorios_total,
         capacidade_semiaberto = x1_3_capacidade_do_estabelecimento_regime_semiaberto_total,
         capacidade_aberto = x1_3_capacidade_do_estabelecimento_regime_aberto_total,
         capacidade_fechado = x1_3_capacidade_do_estabelecimento_regime_fechado_total,
         ppl_total = x4_1_populacao_prisional_total,
         ppl_provisorios = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_total,
         ppl_fechado = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_total,
         ppl_semiaberto = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_total,
         ppl_aberto = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_total,
         ppl_escola_analfabeto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_analfabeto_total,
         ppl_escola_alfabetizado = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_alfabetizado_sem_cursos_regulares_total,
         ppl_escola_fundincompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_fundamental_incompleto_total,
         ppl_escola_fundcompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_fundamental_completo_total,
         ppl_escola_medioincompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_medio_incompleto_total,
         ppl_escola_mediocompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_medio_completo_total,
         ppl_escola_supeincompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_superior_incompleto_total,
         ppl_escola_supecompleto = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_superior_completo_total,
         ppl_escola_acimasuperior = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_ensino_acima_de_superior_completo_total,
         ppl_escola_total = x5_6_quantidade_de_pessoas_privadas_de_liberdade_por_grau_de_instrucao_total,
         ppl_deficiencia = x5_5_pessoas_com_deficiencia_total_de_pessoas_privadas_de_liberdade_com_deficiencia_total,
         ppl_raca_branca = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_branca_total,
         ppl_raca_preta = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_preta_total,
         ppl_raca_parda = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_parda_total,
         ppl_raca_amarela = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_amarela_total,
         ppl_raca_indigena = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_indigena_total,
         ppl_raca_total = x5_2_quantidade_de_pessoas_privadas_de_liberdade_por_cor_de_pele_raca_etnia_total,
         docs_estab = x5_7_numero_de_pessoas_privadas_de_liberdade_com_documentos_pessoais_o_estabelecimento_possui_a_documentacao_fisica_das_pessoas_privadas_de_liberdade,
         docs_sim_masc = x5_7_numero_de_pessoas_privadas_de_liberdade_com_documentos_pessoais_numero_de_pessoas_com_algum_dos_documentos_acima_masculino,
         docs_sim_fem = x5_7_numero_de_pessoas_privadas_de_liberdade_com_documentos_pessoais_numero_de_pessoas_com_algum_dos_documentos_acima_feminino,
         docs_nao_masc = x5_7_numero_de_pessoas_privadas_de_liberdade_com_documentos_pessoais_numero_de_pessoas_sem_documentos_masculino,
         docs_nao_fem = x5_7_numero_de_pessoas_privadas_de_liberdade_com_documentos_pessoais_numero_de_pessoas_sem_documentos_feminino,
         ppl_visitas_cadastro_masc = x5_11_numero_de_pessoas_privadas_de_liberdade_que_possuem_visitantes_cadastrados_pessoas_com_visitantes_cadastrados_masculino,
         ppl_visitas_cadastro_fem = x5_11_numero_de_pessoas_privadas_de_liberdade_que_possuem_visitantes_cadastrados_pessoas_com_visitantes_cadastrados_feminino,
         ppl_visitas_masc = x7_3_quantidade_de_visitas_registradas_no_periodo_de_referencia_quantidade_de_presos_que_receberam_visita_no_periodo_de_referencia_masculino,
         ppl_visitas_fem = x7_3_quantidade_de_visitas_registradas_no_periodo_de_referencia_quantidade_de_presos_que_receberam_visita_no_periodo_de_referencia_feminino,
         visitas_total = x7_3_quantidade_de_visitas_registradas_no_periodo_de_referencia_quantidade_de_visitas_registradas_no_periodo_de_referencia,
         obitos_total = x4_5_movimentacao_no_sistema_prisional_total_do_periodo_de_referencia_saidas_total_de_obitos_total,
         obitos_saude = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_naturais_obitos_por_motivos_de_saude_total,
         obitos_criminal = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_criminais_total,
         obitos_suicidios = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_suicidios_total,
         obitos_acidentes = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_acidentais_total,
         obitos_desconhecida = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_com_causa_desconhecida_total,
         consultorio_medico_v = x2_4_modulo_de_saude_consultorio_medico,
         consultorio_medico_q = x2_4_modulo_de_saude_consultorio_medico_quantidade,
         consultorio_odonto_v = x2_4_modulo_de_saude_consultorio_odontologico,
         consultorio_odonto_q = x2_4_modulo_de_saude_consultorio_odontologico_quantidade,
         consultorio_farmacia_v = x2_4_modulo_de_saude_farmacia_ou_sala_de_estoque_dispensacao_de_medicamentos,
         consultorio_farmacia_q = x2_4_modulo_de_saude_farmacia_ou_sala_de_estoque_dispensacao_de_medicamentos_quantidade,
         profissional_saude_enfermeiro = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_enfermeiros_total,
         profissional_saude_auxiliar_enfermagem = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_auxiliar_e_tecnico_de_enfermagem_total,
         profissional_saude_psicologo = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_psicologos_total,
         profissional_saude_dentista = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_dentistas_total,
         profissional_saude_auxiliar_odonto = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_tecnico_auxiliar_odontologico_total,
         profissional_saude_medico_clinico_geral = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_medicos_clinicos_gerais_total,
         profissional_saude_medico_ginecologista = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_medicos_ginecologistas_total,
         profissional_saude_medico_psquiatra = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_medicos_psiquiatras_total,
         profissional_saude_medico_outras_especialidades = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_medicos_outras_especialidades_total,
         profissional_saude_terapeuta_ocupacional = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_terapeuta_terapeuta_ocupacional_total,
         consultas_medicas_externas = x6_6_informacoes_da_area_de_saude_total_do_periodo_consultas_medicas_realizadas_externamente_total,
         consultas_medicas_local = x6_6_informacoes_da_area_de_saude_total_do_periodo_consultas_medicas_realizadas_no_estabelecimento_total,
         consultas_psicologicas = x6_6_informacoes_da_area_de_saude_total_do_periodo_consultas_psicologicas_total,
         consultas_odontologicas = x6_6_informacoes_da_area_de_saude_total_do_periodo_consultas_odontologicas_total,
         consultas_exames_testes = x6_6_informacoes_da_area_de_saude_total_do_periodo_quantidade_de_exames_e_testagem_total,
         consultas_cirurgias = x6_6_informacoes_da_area_de_saude_total_do_periodo_quantidade_de_intervencoes_cirurgicas_total,
         consultas_vacinas = x6_6_informacoes_da_area_de_saude_total_do_periodo_quantidade_de_vacinas_total,
         consultas_curativos = x6_6_informacoes_da_area_de_saude_total_do_periodo_quantidade_de_outros_procedimentos_como_sutura_e_curativo_total,
         agravos_hiv = x6_7_quantidade_de_pessoas_com_agravos_transmissiveis_na_data_de_fim_do_periodo_de_referencia_hiv_total,
         agravos_sifilis = x6_7_quantidade_de_pessoas_com_agravos_transmissiveis_na_data_de_fim_do_periodo_de_referencia_sifilis_total,
         agravos_hepatite = x6_7_quantidade_de_pessoas_com_agravos_transmissiveis_na_data_de_fim_do_periodo_de_referencia_hepatite_total,
         agravos_tuberculose = x6_7_quantidade_de_pessoas_com_agravos_transmissiveis_na_data_de_fim_do_periodo_de_referencia_tuberculose_total,
         agravos_outros = x6_7_quantidade_de_pessoas_com_agravos_transmissiveis_na_data_de_fim_do_periodo_de_referencia_outros_total,
         servidores_custodia = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_servidor_voltado_a_atividade_de_custodia_total,
         servidores_total = x3_1_quantidade_de_servidores_que_atuam_no_sistema_prisional_total,
         laborterapia_interno_masc = x6_1_quantidade_de_pessoas_privadas_de_liberdade_em_programas_de_laborterapia_trabalho_interno_masculino,
         laborterapia_interno_fem = x6_1_quantidade_de_pessoas_privadas_de_liberdade_em_programas_de_laborterapia_trabalho_interno_feminino,
         laborterapia_externo_masc = x6_1_quantidade_de_pessoas_privadas_de_liberdade_em_programas_de_laborterapia_trabalho_externo_masculino,
         laborterapia_externo_fem = x6_1_quantidade_de_pessoas_privadas_de_liberdade_em_programas_de_laborterapia_trabalho_externo_feminino,
         educacao_alfabetizacao = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_alfabetizacao_total,
         educacao_ens_funda = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_ensino_fundamental_total,
         educacao_ens_medio = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_ensino_medio_total,
         educacao_ens_superior = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_ensino_superior_total,
         educacao_ens_tecnico = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_curso_tecnico_acima_de_800_horas_de_aula_total,
         educacao_form_inicial = x6_3_quantidade_de_pessoas_privadas_de_liberdade_em_atividade_educacional_curso_de_formacao_inicial_e_continuada_capacitacao_profissional_acima_de_160_horas_de_aula_total
  ) %>% 
  writexl::write_xlsx('data/dados_infopen_goias.xlsx')


## Dados do CNIEP

library(readr)

historico_fugas <- read_delim("data-raw/cniep/02032023_historico_fugas.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names()

historico_presos <- read_delim("data-raw/cniep/02032023_historico_presos.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names()


cniep_go <- merge(historico_fugas, historico_presos, by.x = c('estabelecimento','mes_ano_inspecao'), by.y = c('estabelecimento','mes_ano_inspecao')) %>% 
  filter(uf.x == "GO") %>% 
  writexl::write_xlsx('data/cniep_goias.xlsx')

unidades <- merge(historico_fugas, historico_presos, by.x = c('estabelecimento','mes_ano_inspecao'), by.y = c('estabelecimento','mes_ano_inspecao')) %>% 
  filter(uf.x == "GO") %>% 
  select(estabelecimento) %>% 
  unique() %>% 
  writexl::write_xlsx('data/cniep_unidades_goias.xlsx')


## dados brasil

df_brasil <- df_infopen_microdados %>% 
  clean_names() %>% 
  arrange(uf, ano, semestre) %>% 
  select(uf, 
         ano, 
         semestre, 
         nome_do_estabelecimento, 
         capacidade_masc = x1_3_capacidade_do_estabelecimento_masculino_total,
         capacidade_fem = x1_3_capacidade_do_estabelecimento_feminino_total,
         ppl_total = x4_1_populacao_prisional_total,
         ppl_provisorios = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_total,
         ppl_fechado = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_total,
         ppl_semiaberto = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_total,
         ppl_aberto = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_total,
         obitos_total = x4_5_movimentacao_no_sistema_prisional_total_do_periodo_de_referencia_saidas_total_de_obitos_total,
         obitos_saude = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_naturais_obitos_por_motivos_de_saude_total,
         obitos_criminal = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_criminais_total,
         obitos_suicidios = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_suicidios_total,
         obitos_acidentes = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_acidentais_total,
         obitos_desconhecida = x6_8_mortalidade_no_sistema_prisional_total_do_periodo_obitos_com_causa_desconhecida_total
  )

df_brasil2 <- df_brasil %>% 
  mutate(populacao = as.numeric(ppl_total)) %>% 
  group_by(uf, ano, semestre) %>% 
  summarise(pop.total = sum(ppl_total,na.rm = TRUE),
            capacidade = sum(capacidade_fem, na.rm = TRUE) + sum(capacidade_masc, na.rm = TRUE), 
            mortalidade = (sum(obitos_total, na.rm = TRUE)*100000)/pop.total) %>% 
  writexl::write_xlsx('data/dados_infopen_brasil.xlsx')













