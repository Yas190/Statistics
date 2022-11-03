library(tibble)
library(dplyr)
library(tidyverse)

enem_2021 <- as_tibble(
  read.csv2("C:\\Users\\yasmin.pires\\OneDrive - Iteris Consultoria e Software\\Documentos\\Pessoal\\R\\microdados_enem_2021\\DADOS\\MICRODADOS_ENEM_2021.csv")
)

enem_2021_range <- enem_2021 %>%
  select(
    NU_INSCRICAO,
    TP_SEXO,
    TP_COR_RACA,
    TP_DEPENDENCIA_ADM_ESC,
    starts_with("TP_PRESENCA_"),
    Q006,
    NU_NOTA_CN,
    NU_NOTA_CH,
    NU_NOTA_LC,
    NU_NOTA_MT
    ) %>%
  mutate(
    across(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT), as.numeric)
    ) %>%
  mutate(
    across(c(NU_INSCRICAO, TP_SEXO, TP_COR_RACA, TP_DEPENDENCIA_ADM_ESC, starts_with("TP_PRESENCA_")), as.character)
  )

enem_2021_range <-enem_2021_range %>%
  mutate(
    MEDIA_GERAL = rowMeans(select(enem_2021_range, starts_with("NU_NOTA_")))
    ) %>%
  filter(
    !is.na(MEDIA_GERAL)
    ) %>%
  mutate(
    TP_COR_RACA = recode(TP_COR_RACA, "0"= "Não Declarado", "1"= "Branca", "2"= "Preta", "3"= "Parda", "4"= "Amarela", "5"= "Indigena", "6" = "Nao Dispoe"),
    TP_DEPENDENCIA_ADM_ESC = recode(TP_DEPENDENCIA_ADM_ESC, "1" = "Federal", "2" = "Estadual", "3" = "Municipal", "4" = "Privada")
  )


