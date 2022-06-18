# baixar os arquivos ------------------------------------------------------

library(rvest)

url_siglas <- "https://www.todamateria.com.br/siglas-estados-brasileiros/" %>%
  read_html()

siglas <- html_table(url_siglas, fill = TRUE)
siglas <- siglas[[1]] %>% 
  select(Sigla) %>% as.vector()

# [necessidade de mudar a extensão do arquivo de acordo com o ano - pdf a partir de 21]

for (i in siglas$Sigla) {
  url <- paste0("https://www.gov.br/depen/pt-br/servicos/sisdepen/mais-informacoes/relatorios-infopen/relatorios-analiticos/",i,"/",tolower(i),"-dez-2021.pdf")
  
  httr::GET(url, httr::write_disk(path = paste0("data/relatorios/",i,"_dez_2021.pdf"), overwrite = TRUE))
}



# ler os arquivos em .xls -------------------------------------------------

vars <- c("População carcerária",
          "População carcerária por 100.000 habitantes",
          "Quantidade de Presos (Polícia e Segurança Pública)",
          "Quantidade de Presos custodiados no Sistema Penitenciário")

excel <- list.files(path = "data/relatorios", 
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = "(0|1|9|8|7)\\.xls") %>% 
  tbl_df() %>% 
  mutate(sheetName = map(value, readxl::excel_sheets)) %>%
  unnest(sheetName) %>% 
  mutate(myFiles = purrr::map2(value, sheetName, function(x,y) {
    readxl::read_excel(x, sheet = paste(y))})) %>% 
  unnest(myFiles) %>% 
  select(-sheetName) %>% 
  mutate(ciclo = stringr::str_extract(value,"(jun|dez)\\_20(20|21|19|18|17)")) %>% 
  mutate(UF = stringr::str_extract(value, "\\/\\w{2}\\_")) %>% 
  mutate(UF = stringr::str_remove_all(UF, "(\\/|\\_)")) %>% 
  janitor::clean_names() %>% 
  rename(var = ministerio_da_justica_departamento_penitenciario_nacional_sistema_de_informacoes_do_departamento_penitenciario_nacional_sisdepen) %>% 
  select(-value) %>% 
  select(uf,ciclo,var,everything()) %>% 
  filter(var %in% vars) %>% 
  select(uf,ciclo,var,x11,x13,x15) %>% 
  rename ("homens"= x11, "mulheres" = x13, "total" = x15)

# ler os arquivos em pdf --------------------------------------------------

library(pdftools)
library(stringr)

pdfs <- list.files(path = "data/relatorios", 
                    full.names = TRUE,
                    recursive = TRUE,
                    pattern = "\\.pdf") %>% 
  tbl_df() %>% 
  mutate(texto_pdf = map(value, pdftools::pdf_text)) %>% 
  mutate(texto_pdf = stringr::str_squish(texto_pdf)) %>% 
  mutate(ciclo = stringr::str_extract(texto_pdf,"(jan|jul)\\-(jun|dez)\\s202\\d{1}")) %>% 
  mutate(pop_total = stringr::str_extract(texto_pdf,"População\\scarcerária\\s(\\d|\\.)+")) %>% 
  mutate(pop_senasp = stringr::str_extract(texto_pdf,"Quantidade\\sde\\sPresos\\s\\(Polícia\\se\\sSegurança\\sPública\\)\\s(\\d|\\.|\\s)+")) %>% 
  mutate(pop_infopen = stringr::str_extract(texto_pdf,"Quantidade\\sde\\sPresos\\scustodiados\\sno\\sSistema\\sPenitenciário\\s(\\d|\\.|\\s)+")) %>% 
  mutate(taxa_encar = stringr::str_extract(texto_pdf, "População\\scarcerária\\spor.+\\d+\\,\\d{2}.+\\d{3}\\.\\d{3}\\shabitantes")) %>% 
  select(-texto_pdf) %>% 
  mutate(pop_total = str_extract(pop_total,"(\\d|\\.)+")) %>% 
  mutate(pop_senasp = str_extract(pop_senasp,"(\\d|\\.)+\\s*(\\d|\\.)*\\s*(\\d|\\.)*")) %>% 
  mutate(pop_senasp = str_split(pop_senasp,boundary("word"),simplify = TRUE)) %>% 
  mutate(pop_infopen = str_extract(pop_infopen,"(\\d|\\.)+\\s(\\d|\\.)+\\s(\\d|\\.)+")) %>% 
  mutate(pop_infopen = str_split(pop_infopen, "\\s",simplify = TRUE)) %>% 
  mutate(taxa_encar = str_extract(taxa_encar,"\\d{1,3}\\,\\d{2}")) %>% 
  mutate(pop_senasp_h = pop_senasp[,1]) %>% 
  mutate(pop_senasp_m = pop_senasp[,2]) %>% 
  mutate(pop_senasp_total = pop_senasp[,3]) %>% 
  mutate(pop_infopen_h = pop_infopen[,1]) %>% 
  mutate(pop_infopen_m = pop_infopen[,2]) %>% 
  mutate(pop_infopen_total = pop_infopen[,3]) %>% 
  mutate(pop_senasp_total = data.table::fifelse(pop_senasp_total>0,pop_senasp_total,pop_senasp_h)) %>% 
  mutate(ciclo = stringr::str_extract(value,"(jun|dez)\\_20(20|21|19|18|17)")) %>% 
  mutate(uf = stringr::str_extract(value, "\\/\\w{2}\\_")) %>% 
  mutate(uf = stringr::str_remove_all(uf, "(\\/|\\_)")) %>% 
  select(uf,ciclo,pop_total,taxa_encar,pop_senasp_h:pop_infopen_total) %>% 
  pivot_longer(-c("uf","ciclo"), names_to = "var", values_to = "total") %>% 
  mutate(var = case_when(
    str_detect(var, "pop_total") ~ "População carcerária",
    str_detect(var, "taxa_encar") ~ "População carcerária por 100.000 habitantes",
    str_detect(var, "pop_senasp_total") ~ "Quantidade de Presos (Polícia e Segurança Pública)",
    str_detect(var, "pop_infopen_total") ~ "Quantidade de Presos custodiados no Sistema Penitenciário",
    str_detect(var, "pop_senasp_h") ~ "Quantidade de Presos (Polícia e Segurança Pública) - Homens",
    str_detect(var, "pop_senasp_m") ~ "Quantidade de Presos (Polícia e Segurança Pública) - Mulheres",
    str_detect(var, "pop_infopen_h") ~ "Quantidade de Presos custodiados no Sistema Penitenciário - Homens",
    str_detect(var, "pop_infopen_m") ~ "Quantidade de Presos custodiados no Sistema Penitenciário - Mulheres",
    TRUE ~ NA_character_
  ))

homens <- pdfs %>% 
  filter(str_detect(var, "Homens")) %>% 
  mutate(var = str_remove(var, "\\s\\-\\sHomens")) %>% 
  rename(homens = total)

mulheres <- pdfs %>% 
  filter(str_detect(var, "Mulheres")) %>% 
  mutate(var = str_remove(var, "\\s\\-\\sMulheres")) %>% 
  rename(mulheres = total)

pdfs <- pdfs %>% filter(!str_detect(var, "Homens|Mulheres")) %>% left_join(.,homens) %>% left_join(.,mulheres) %>% 
  mutate(total = str_replace(total,",",".")) %>% 
  select(uf:var,homens,mulheres,total)

  
# tabelas finais ----------------------------------------------------------

# gerar o arquivo final para analise

tabela <- rbind(excel,pdfs)

# tabela de populacao total

pop_total <- tabela %>% 
  filter(stringr::str_detect(ciclo, "dez")) %>% 
  mutate(ano = stringr::str_extract(ciclo, "\\d{4}")) %>% 
  filter(var == "População carcerária") %>% 
  select(uf,ano,total) %>% 
  pivot_wider(names_from = ano, values_from = total)

# tabela de populacao senasp

pop_senasp <- tabela %>% 
  filter(stringr::str_detect(ciclo, "dez")) %>% 
  mutate(ano = stringr::str_extract(ciclo, "\\d{4}")) %>% 
  filter(var == "Quantidade de Presos (Polícia e Segurança Pública)") %>% 
  select(uf,ano,total) %>% 
  pivot_wider(names_from = ano, values_from = total)

# tabela de populacao infopen

pop_infopen <- tabela %>% 
  filter(stringr::str_detect(ciclo, "dez")) %>% 
  mutate(ano = stringr::str_extract(ciclo, "\\d{4}")) %>% 
  filter(var == "Quantidade de Presos custodiados no Sistema Penitenciário") %>% 
  select(uf,ano,total) %>% 
  pivot_wider(names_from = ano, values_from = total)

# tabela de taxa de encarceramento

taxa_encar <- tabela %>% 
  filter(stringr::str_detect(ciclo, "dez")) %>% 
  mutate(ano = stringr::str_extract(ciclo, "\\d{4}")) %>% 
  filter(var == "População carcerária por 100.000 habitantes") %>% 
  select(uf,ano,total) %>% 
  mutate(total = as.numeric(total)) %>% 
  mutate(total = format(round(total,digits = 2))) %>% 
  pivot_wider(names_from = ano, values_from = total)

