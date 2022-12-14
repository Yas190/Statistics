# Carregando Pacotes
library(tibble)
library(dplyr)
library(tidyverse)

# Importando Base de Dados
enem_2021 <- as_tibble(
  read.csv("C:\\Users\\yasmin.pires\\OneDrive - Iteris Consultoria e Software\\Documentos\\Pessoal\\R\\Curso\\dados filtrados.csv")
)

# Selecionando Colunas de interesse
enem_2021_range <- enem_2021 %>%
  mutate(
    across(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, starts_with("TP_PRESENCA_")), as.numeric)
    ) %>%
  mutate(
    across(c(NU_INSCRICAO, TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC,), as.character)
  )

inscritos_enem_2021 <- nrow(enem_2021_range)

presencas_1_2021 <- sum(enem_2021_range$TP_PRESENCA_LC == 1 & enem_2021_range$TP_PRESENCA_CH == 1)
faltantes_1_2021 <- inscritos_enem_2021 - presencas_1_2021

presencas_2_2021 <- sum(enem_2021_range$TP_PRESENCA_CN == 1 & enem_2021_range$TP_PRESENCA_MT == 1)
faltantes_2_2021 <- inscritos_enem_2021 - presencas_2_2021

presenca_1_2_2021 <- sum(
  enem_2021_range$TP_PRESENCA_LC == 1 & enem_2021_range$TP_PRESENCA_CH == 1 & enem_2021_range$TP_PRESENCA_CN == 1 & enem_2021_range$TP_PRESENCA_MT == 1)

enem_2021_range <-enem_2021_range %>%
  mutate(
    MEDIA_GERAL = rowMeans(select(enem_2021_range, starts_with("NU_NOTA_")))
    ) %>%
  filter(
    !is.na(MEDIA_GERAL))

sexo_feminino_2021 <- sum(enem_2021_range$TP_SEXO == "F")
sexo_masculino_2021 <- sum(enem_2021_range$TP_SEXO == "M")

cor_preta_2021 <- sum(enem_2021_range$TP_COR_RACA == "Preta")
cor_branca_2021 <- sum(enem_2021_range$TP_COR_RACA == "Branca")
cor_nao_declarado_2021 <- sum(enem_2021_range$TP_COR_RACA == "Nao Declarado")
cor_parda_2021 <- sum(enem_2021_range$TP_COR_RACA == "Parda")
cor_amarela_2021 <- sum(enem_2021_range$TP_COR_RACA == "Amarela")
cor_indigena_2021 <- sum(enem_2021_range$TP_COR_RACA == "Indigena")
cor_nao_dispoe_2021 <- sum(enem_2021_range$TP_COR_RACA == "Nao Dispoe")

media_faixa1_2021 <- sapply(
  enem_2021_range[enem_2021_range$Q006 == "A"|enem_2021_range$Q006 == "B"|enem_2021_range$Q006 == "C"|enem_2021_range$Q006 == "D",
                  "MEDIA_GERAL"], mean)
media_faixa2_2021 <- sapply(
  enem_2021_range[enem_2021_range$Q006 == "E"|enem_2021_range$Q006 == "F"|enem_2021_range$Q006 == "G"|enem_2021_range$Q006 == "H",
                  "MEDIA_GERAL"], mean)
media_faixa3_2021 <- sapply(
  enem_2021_range[enem_2021_range$Q006 == "I"|enem_2021_range$Q006 == "J"|enem_2021_range$Q006 == "K"|enem_2021_range$Q006 == "L",
                  "MEDIA_GERAL"], mean)
media_faixa4_2021 <- sapply(
  enem_2021_range[enem_2021_range$Q006 == "M"|enem_2021_range$Q006 == "N"|enem_2021_range$Q006 == "O"|enem_2021_range$Q006 == "P",
                  "MEDIA_GERAL"], mean)

media_federal_2021 <- sapply(
  enem_2021_range[enem_2021_range$TP_DEPENDENCIA_ADM_ESC == "Federal",
                  "MEDIA_GERAL"], mean)
media_estadual_2021 <- sapply(
  enem_2021_range[enem_2021_range$TP_DEPENDENCIA_ADM_ESC == "Estadual",
                  "MEDIA_GERAL"], mean)
media_municipal_2021 <- sapply(
  enem_2021_range[enem_2021_range$TP_DEPENDENCIA_ADM_ESC == "Municipal",
                  "MEDIA_GERAL"], mean)
media_municipal_2021 <- sapply(
  enem_2021_range[enem_2021_range$TP_DEPENDENCIA_ADM_ESC == "Privada",
                  "MEDIA_GERAL"], mean)

media_CN_2021 <- mean(enem_2021_range$NU_NOTA_CN)
media_CH_2021 <- mean(enem_2021_range$NU_NOTA_CH)
media_LC_2021 <- mean(enem_2021_range$NU_NOTA_LC)
media_MT_2021 <- mean(enem_2021_range$NU_NOTA_MT)

