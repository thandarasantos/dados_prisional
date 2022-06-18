library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

# ler os arquivos de microdados -------------------------------------------

df <- list.files(path = "data/microdados", 
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = "dados.+(0|1|9|8|7)\\.xlsx") %>% 
  tbl_df() %>% 
  mutate(sheetName = map(value, readxl::excel_sheets)) %>%
  unnest(sheetName) %>% 
  mutate(myFiles = purrr::map2(value, sheetName, function(x,y) {
    readxl::read_excel(x, sheet = paste(y))})) %>% 
  unnest(myFiles) %>% 
  select(-sheetName) %>% 
  mutate(ciclo = stringr::str_extract(value,"(jan|jul)\\-(jun|dez)\\-20(20|21|19|18|17)")) %>% 
  select(-value) %>% 
  mutate(ano = stringr::str_extract(ciclo, "\\d{4}"))

# renomear variaveis
dicionario <- readxl::read_xlsx("data/dicionario_infopen.xlsx")

old <- as_vector(dicionario$var_texto)
new <- as_vector(dicionario$id)

df <- data.table::setnames(df,old = old, new = new)

# tabelas finais ----------------------------------------------------------

# tabela de obitos
obitos <- df %>% 
  group_by(v0010,ano) %>% 
  summarise(obitos_naturais = sum(v1306, na.rm = TRUE), 
            obitos_criminais = sum(v1309, na.rm = TRUE),
            suicidios = sum(v1312, na.rm = TRUE),
            obitos_acidentais = sum(v1315, na.rm = TRUE),
            obitos_desconhec = sum(v1318, na.rm = TRUE))


