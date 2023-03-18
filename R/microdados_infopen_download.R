library(tidyverse)
library(rvest)

# url do formulário: https://www.gov.br/depen/pt-br/servicos/sisdepen/relatorios-e-manuais/bases-de-dados/arquivos/formulario-sobre-informacoes-prisionais.pdf

# url dos microdados do infopen
url_dados_infopen <- "https://www.gov.br/depen/pt-br/servicos/sisdepen/relatorios-e-manuais/bases-de-dados"

# obtem o url de cada arquivo publicado
urls_microdados <- url_dados_infopen %>% 
  read_html() %>% 
  html_nodes(xpath = "//table//a") %>% 
  html_attr(name = "href") 
  
# funcao para download do microdado a partir do url
download_microdado <- function(x) {
  
  nome_arquivo <- str_replace(x, "https://www.gov.br/depen/pt-br/servicos/sisdepen/relatorios-e-manuais/bases-de-dados/", "")
  
  print(nome_arquivo)
  
  httr::GET(x, httr::write_disk(path = paste0("data-raw/microdados2/", nome_arquivo), overwrite = TRUE))
  
} 

# download de todos os arquivos em sequencia
urls_microdados %>% 
  map(download_microdado)
