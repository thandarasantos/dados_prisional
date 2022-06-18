library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)

# ler os arquivos de microdados -------------------------------------------

cniep <- list.files(path = "data-raw/cniep", 
                 full.names = TRUE,
                 recursive = TRUE, 
                 pattern = ".xlsx") %>% 
  tbl_df() %>% 
  mutate(sheetName = map(value, readxl::excel_sheets)) %>%
  unnest(sheetName) %>% 
  mutate(myFiles = purrr::map2(value, sheetName, function(x,y) {
    readxl::read_excel(x, sheet = paste(y))})) %>% 
  unnest(myFiles) %>% 
  select(-sheetName) %>% 
  rename(situacao = ...4) %>% 
  janitor::clean_names()


# tabelas finais ----------------------------------------------------------

# salvar o arquivo em .rda para o projeto

save(cniep,file = "data/cniep.rda")

# tabela de obitos
obitos <- df %>% 
  group_by(v0010,ano) %>% 
  summarise(obitos_naturais = sum(v1306, na.rm = TRUE), 
            obitos_criminais = sum(v1309, na.rm = TRUE),
            suicidios = sum(v1312, na.rm = TRUE),
            obitos_acidentais = sum(v1315, na.rm = TRUE),
            obitos_desconhec = sum(v1318, na.rm = TRUE))  


