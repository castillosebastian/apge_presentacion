# Informe 
library(dplyr)
library(knitr)
library(apgyeDSL)
library(apgyeJusEROrganization)
library(apgyeOperationsJusER)
library(stringr)
library(janitor)
library(stringr)
library(kableExtra)
library(tidyr)
library(tibble)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(lubridate)
source('resoluciones_v3.R')

# Listado de Magistrados y Funcionarios
magistrados_y_secretarios <- DB_PROD() %>% apgyeDSL::apgyeTableData('personal_planta_ocupada') %>%
  left_join(DB_PROD() %>% apgyeDSL::apgyeTableData('personal_personas') %>%
              select(idagente, categoria, apellido, nombres), by=c("idagente")
  ) %>%
  filter(grepl("JUEZ|SECRETARIO|VOCAL|FISCAL|DEFENSOR", categoria)) %>%
  collect()
magist_func_id_agentes <- magistrados_y_secretarios$idagente
# add "0" ti IDs for External Agent
magist_func_id_agentes[length(magist_func_id_agentes)+1] <- "0"



