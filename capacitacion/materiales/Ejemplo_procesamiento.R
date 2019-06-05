source("~/apge_presentacion/capacitacion/materiales/informe.R")

primarias <- DB_PROD() %>% 
  apgyeTableData(CADR1C) %>% 
  apgyeDSL::interval("2019-04-01", "2019-05-01") %>% 
  collect()

procesado <- DB_PROD() %>% 
  apgyeTableData(CADR1C) %>% 
  apgyeDSL::interval("2019-04-01", "2019-05-01") %>% 
  group_by(iep, data_interval_start, data_interval_end) %>% 
  process() %>%
  .$result

# Imputación con Evaluaciones según lógicas del negocio

causas_resueltas <- resultado %>%
  filter(!is.na(fres)) %>% 
  mutate(control_intervalo =  fres >= data_interval_start & fres < data_interval_end) %>% 
  mutate(control_mmp = tres != 0 | is.na(tres)) %>% 
  mutate(
    atermino = (fres <= fvenc1 & control_intervalo & control_mmp), 
    res_luego1venc = (fres > fvenc1 & control_intervalo & control_mmp))
