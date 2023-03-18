
library(tidyverse)
library(janitor)
library(readxl)

# listagem de arquivos de microdados baixados com informacao de ano e semestre
arquivos <- 
  tibble(arquivo = list.files('data-raw/microdados2')) %>% 
  filter(stringr::str_detect(arquivo, ".csv")) %>% 
  mutate(
    ano = stringr::str_extract(arquivo, "\\d{4}"), 
    semestre = case_when(
      stringr::str_detect(arquivo, "\\-1\\.csv") ~ 'jan-jun',
      stringr::str_detect(arquivo, "\\-2\\.csv") ~ 'jul-dez',
      T ~ NA_character_
    )) %>% 
  arrange(ano, semestre)

# por enquanto, excluir 2014-1 2015-1 e 2016-1 por incompatibilidade de variaveis

# funcao para ler arquivo de microdados
# insere ano e semestre como variavel
ler_arquivo_microdados <- function(nome_arquivo) {
  
  df <- read.csv2(
    file.path(
      'data-raw/microdados2',
      nome_arquivo
    )
  )

  ano <- arquivos %>% 
    filter(arquivo == nome_arquivo) %>% 
    pull(ano)
  
  semestre <- arquivos %>% 
    filter(arquivo == nome_arquivo) %>% 
    pull(semestre)
  
  df <- df %>% 
    mutate(
      ano = ano,
      semestre = semestre)

  return(df)
    
}

# cria lista com df de todos os microdados
df_list <- arquivos %>% 
  pull(arquivo) %>% 
  map(ler_arquivo_microdados)

# transforma lista em data frame unico de microdados
df_infopen_microdados <- df_list %>% 
  reduce(bind_rows) %>% 
  clean_names()

# cria lista com vetores de nomes de variaveis de cada data frame
varnames_list <- df_list %>% 
  map(names)

# vetor de periodos de arquivos (mesma ordem da lista)
vetor_periodos <- paste0('p_', arquivos$ano, "_", arquivos$semestre)

# produz data frame com coluna de nome de variavel e uma coluna para cada periodo
df_names_list <- 1:length(varnames_list) %>% 
  map(function(i) {
    df <- tibble(var_name = unlist(varnames_list[i]),
                 var = T)
    names(df)[2] <- vetor_periodos[i]
    return(df)
  }
  ) %>% 
  rev %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0)

df_names_list %>%  View

# completa data frame de arquivo para exportar como metadados
arquivos <- arquivos %>% 
  mutate(
    numero_unidades = df_list %>% 
      map(nrow) %>% 
      unlist,
    numero_variaveis = varnames_list %>% 
      map(length) %>% 
      unlist
  ) %>% 
  complete(ano, semestre)

# exporta data frame de metadados
arquivos %>% 
  write_csv('data/microdados2_infopen_metadados.csv')

# exporta df de variaveis por periodo
df_names_list %>% 
  write_csv('data/microdados2_infopen_variaveis_por_arquivo.csv')

# exporta data frame unico de microdados
df_infopen_microdados %>% 
  write_csv('data/microdados2_infopen.csv')