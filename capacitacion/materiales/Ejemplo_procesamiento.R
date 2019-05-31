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