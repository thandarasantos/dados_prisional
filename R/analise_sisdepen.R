## Quantidade de unidades prisionais existentes no país

base_estabelecimento %>%
  dplyr::filter(situacao_estabelecimento == "Ativo") %>%
  dplyr::select(id_ano_sisdepen:nome_estabelecimento,
                ambito_federativo,
                sigla_uf) %>%
  dplyr::count(id_ano_sisdepen, id_mes_sisdepen) %>%
  kableExtra::kable(col.names = c("Ano", "Mês", "Quantidade"), format.args = list(big.mark = ".")) 



### Distribuição das unidades por estados

base_estabelecimento %>% 
  dplyr::filter(id_ano_sisdepen %in% c(2014, 2020)) %>% 
  dplyr::group_by(id_ano_sisdepen, sigla_uf) %>% 
  dplyr::count() %>% 
  tidyr::pivot_wider(names_from = id_ano_sisdepen, values_from = n) %>%
  janitor::adorn_percentages(denominator = "col") %>%
  dplyr::arrange(-`2020`) %>% 
  janitor::adorn_pct_formatting() %>% 
  knitr::kable(col.names = c("UF", "2014", "2020")) 

## Tipo de gestão das unidades

base_estabelecimento %>%
  dplyr::filter(situacao_estabelecimento == "Ativo") %>%
  dplyr::select(id_ano_sisdepen:nome_estabelecimento,
                ambito_federativo, sigla_uf, tipo_gestao) %>%
  dplyr::count(id_ano_sisdepen, id_mes_sisdepen, tipo_gestao) %>%
  dplyr::mutate(
    data_sisdepen = paste0(id_ano_sisdepen, "-", id_mes_sisdepen, "-01"),
    data_sisdepen = lubridate::ymd(data_sisdepen)
  ) %>%
  ggplot(aes(x = data_sisdepen, y = n, fill = tipo_gestao)) +
  geom_col() +
  theme_classic() +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  labs(fill = "Tipo de gestão", 
       y = "Estabelecimentos", 
       x = "Ano",
       title = "Tipo de gestão das unidades prisionais")


base_quantidade_presos <- base_populacao %>%
  # dplyr::filter(id_ano_sisdepen == "2020") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    total_provisorios =
      sum(dplyr::across(
        dplyr::matches("provisorio")), na.rm = TRUE),
    total_rf =
      sum(dplyr::across(
        dplyr::matches("sentenca_rf")), na.rm = TRUE),
    total_rsa =
      sum(dplyr::across(
        dplyr::matches("sentenca_rsa")), na.rm = TRUE),
    total_ra =
      sum(dplyr::across(
        dplyr::matches("sentenca_ra")), na.rm = TRUE),
    total_mseg =
      sum(dplyr::across(
        dplyr::matches("sentenca_medseg")), na.rm = TRUE),
    total_populacao =
      sum(dplyr::across(
        dplyr::matches("quantidade_")), na.rm = TRUE),
    total_condenados =
      sum(dplyr::across(
        dplyr::matches("sentenca|medseg")), na.rm = TRUE),
    total_masc =
      sum(dplyr::across(
        dplyr::matches("_mas$")), na.rm = TRUE),
    total_fem =
      sum(dplyr::across(
        dplyr::matches("_fem$")), na.rm = TRUE),
  ) %>%
  dplyr::select(-dplyr::contains("quantidade"))
tabela_quantidade_presos_uf <- base_quantidade_presos %>%
  dplyr::filter(id_ano_sisdepen == 2020) %>%
  dplyr::group_by(sigla_uf) %>%
  dplyr::summarise(
    sum_total_condenados = sum(total_condenados),
    sum_total_provisorios = sum(total_provisorios),
    sum_total_pop = sum(total_populacao),
    sum_total_masc = sum(total_masc),
    sum_total_fem = sum(total_fem),
    pct_provisorios = sum_total_provisorios/sum_total_pop,
    pct_condenados = sum_total_condenados/sum_total_pop,
    pct_masc = sum_total_masc/sum_total_pop,
    pct_fem = sum_total_fem/sum_total_pop
  )


## Quantidade de presos provisórios por Estado

tabela_quantidade_presos_uf %>%
  dplyr::select(sigla_uf, pct_provisorios) %>%
  # tidyr::pivot_longer(names_to = "tipo_prisao",
  #                     cols = pct_provisorios:pct_condenados) %>%
  dplyr::mutate(sigla_uf = forcats::fct_reorder(
    .desc = TRUE,
    sigla_uf,
    pct_provisorios)) %>%
  ggplot(aes(x = sigla_uf, y = pct_provisorios, fill = pct_provisorios)) +
  geom_col(show.legend = FALSE) +
  theme_classic() +
  scale_fill_viridis_c(direction = -1, option = "magma") +
  labs(x = "Estados", y = "% de provisórios",
       title = "Proporção de presos provisórios em relação ao total") +
  scale_y_continuous(labels = scales::percent)

## Comparação entre homens e mulheres presos

mapa_mulheres <- mapa_br %>% 
  dplyr::left_join(
    y = tabela_quantidade_presos_uf %>% 
      dplyr::select(sigla_uf, pct_fem), 
    by = c("abbrev_state" = "sigla_uf")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_fem), show.legend = F) +
  geom_sf_text(mapping = aes(label = abbrev_state), size = 2, 
               color = "grey") +
  scale_fill_gradient(low = "#a88bc4", high = "#1d0338") +
  tema_mapa() +
  labs(title = "Proporção de mulheres presas")

mapa_homens <- mapa_br %>% 
  dplyr::left_join(
    y = tabela_quantidade_presos_uf %>% 
      dplyr::select(sigla_uf, pct_masc), 
    by = c("abbrev_state" = "sigla_uf")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_masc), show.legend = F) +
  geom_sf_text(mapping = aes(label = abbrev_state), size = 2, 
               color = "black") +
  scale_fill_gradient(low = "#ffd8a8", high = "#b06100") +
  tema_mapa() +
  labs(title = "Proporção de homens presos")
mapa_homens + mapa_mulheres

## Distribuição racial nos presídios

tabela_raca <- base_perfilpessoal %>% 
  dplyr::filter(id_ano_sisdepen == 2020) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    pop_brancos = sum(dplyr::across(
      dplyr::matches("raca_branca")), na.rm = TRUE),
    pop_negros = sum(dplyr::across(
      dplyr::matches("raca_preta|raca_parda")), na.rm = TRUE),
    pop_amarela = sum(dplyr::across(
      dplyr::matches("raca_amarela")), na.rm = TRUE),
    pop_indigena = sum(dplyr::across(
      dplyr::matches("raca_branca")), na.rm = TRUE),
    pop_naoinformada = sum(dplyr::across(
      dplyr::matches("raca_naoinformada")), na.rm = TRUE)
  ) %>% 
  dplyr::select(sigla_uf, dplyr::starts_with("pop_"))
tabela_raca %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(brancos = sum(pop_brancos),
                   negros = sum(pop_negros),
                   amarela = sum(pop_amarela),
                   indigena = sum(pop_indigena),
                   na = sum(pop_naoinformada)) %>% 
  tidyr::pivot_longer(cols = dplyr::everything()) %>% 
  ggplot(aes(x = forcats::fct_reorder(name, value, .desc = T),
             y = value, fill = name)) +
  geom_col(show.legend = F) +
  theme_classic() +
  scale_y_continuous(labels = scales::number_format(
    big.mark = ".", decimal.mark = ",")) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  labs(
    x = "Raça", y = "Quantidade de pessoas",
    title = "Distribuição racial nas unidades prisionais",
    subtitle = "em junho de 2020"
  )

#Fica bastante evidente uma "predileção" por pessoas negras (aqui consideradas pretas e pardas) no sistema penitenciário.

tabela_raca %>% 
  dplyr::group_by(sigla_uf) %>% 
  dplyr::summarise_if(.predicate = is.numeric, sum) %>% 
  janitor::adorn_percentages(denominator = "row") %>% 
  #janitor::adorn_pct_formatting() %>% 
  tidyr::pivot_longer(cols = -sigla_uf) %>% 
  dplyr::mutate(
    name = stringr::str_remove(name, "pop_"),
    sigla_uf = forcats::fct_reorder(sigla_uf, value)
  ) %>% 
  ggplot(aes(x = sigla_uf, y = value, fill = name)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  labs(fill = "Raça/Cor", x = "UF", y = "%",
       title = "Distribuição racial nos Estados") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic()


## Distribuição por faixas etárias

tabela_idade <- base_perfilpessoal %>%
  dplyr::filter(id_ano_sisdepen == 2020) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pop_18a24 = sum(dplyr::across(
      dplyr::matches("18a24")), na.rm = TRUE),
    pop_25a29 = sum(dplyr::across(
      dplyr::matches("25a29")), na.rm = TRUE),
    pop_30a34 = sum(dplyr::across(
      dplyr::matches("30a34")), na.rm = TRUE),
    pop_35a45 = sum(dplyr::across(
      dplyr::matches("35a45")), na.rm = TRUE),
    pop_46a60 = sum(dplyr::across(
      dplyr::matches("46a60")), na.rm = TRUE),
    pop_61a70 = sum(dplyr::across(
      dplyr::matches("61a70")), na.rm = TRUE),
    pop_mais70 = sum(dplyr::across(
      dplyr::matches("mais70")), na.rm = TRUE),
    pop_na = sum(dplyr::across(
      dplyr::matches("idade_naoinformada")), na.rm = TRUE)
  ) %>%
  dplyr::select(sigla_uf, dplyr::starts_with("pop_"))
tabela_idade %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise_if(is.numeric, sum) %>% 
  tidyr::pivot_longer(cols = dplyr::everything()) %>% 
  dplyr::mutate(
    name = stringr::str_remove(name, "pop_")
  ) %>% 
  ggplot(aes(x = name,
             y = value, fill = name)) +
  geom_col(show.legend = F) +
  theme_classic() +
  scale_y_continuous(labels = scales::number_format(
    big.mark = ".", decimal.mark = ",")) +
  labs(title = "Distribuição em faixas etárias",
       subtitle = "em junho/2020",
       x = "Faixa etária",
       y = "Quantidade de pessoas") +
  scale_fill_brewer(palette = "Set1")

#Fica claro, por fim, a grande quantidade de jovens no sistema: a maior parte das pessoas presas está na faixa abaixo dos 30 anos de idade.
