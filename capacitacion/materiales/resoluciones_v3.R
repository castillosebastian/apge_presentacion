
## Causas a Despacho y Resueltas 
# Civiles
resoluciones_cco <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo)
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }

  resultado <- resultado %>% 
    process() %>%
    .$result %>%
    ungroup()
 
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  # agrego indicador causas a despacho por mes mediante consulta directa base de datos para evitar filtrado de casos en zzz
  if(str_detect(poblacion$organismo[[1]], "jdo")) {
    cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
      apgyeDSL::interval(start_date, end_date) %>% 
      filter(iep %in% poblacion$organismo) %>% 
      mutate(fdesp = dmy(fdesp)) %>% 
      mutate(mes = date_part('month', fdesp)) %>% 
      group_by(iep, mes) %>% 
      summarise(causas_adespacho = n()) %>% 
      collect() %>% 
      filter(mes >= lubridate::month(start_date), 
             mes < lubridate::month(end_date)) %>% 
      mutate(mes = lubridate::month(mes, label = T, abbr = F))
    
    resultado <- resultado %>% 
      select(-causas_adespacho) %>% 
      left_join(cadesp, by = c("iep", "mes"))
    
    resultado
  } else {
    
    cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
      apgyeDSL::interval(start_date, end_date) %>% 
      filter(iep %in% poblacion$organismo) %>% 
      mutate(fdesp1 = dmy(fdesp1)) %>% 
      mutate(mes = date_part('month', fdesp1)) %>% 
      group_by(iep, mes) %>% 
      summarise(causas_adespacho = n()) %>% 
      collect() %>% 
      filter(mes >= lubridate::month(start_date), 
             mes < lubridate::month(end_date)) %>% 
      mutate(mes = lubridate::month(mes, label = T, abbr = F))
    
    resultado <- resultado %>% 
      select(-causas_adespacho) %>% 
      left_join(cadesp, by = c("iep", "mes"))
    
    resultado
    
  }
    
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luego1venc, res_luego2venc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xindeter, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego1venc", replacement="luego_1er_venc") %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego2venc", replacement="luego_2do_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
    resultado <- resultado %>% 
      group_by(circunscripcion, organismo) %>% 
      do(janitor::adorn_totals(.) %>%
           mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), 
                  circunscripcion=.$circunscripcion[1], 
                  organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
      ungroup() %>% 
      select(circunscripcion, organismo, mes, everything()) %>% 
      mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                          "junio", "julio", "agosto", "septiembre", 
                                          "octubre", "noviembre", "diciembre", "Total"),
                          ordered = TRUE))
    resultado
    
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) %>% 
      janitor::adorn_totals("row") 
  }
  
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_1er_venc + luego_2do_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego 1er Vencim" = luego_1er_venc,
           "luego 2do Vencim" = luego_2do_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha")) 
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
# Laborales 
resoluciones_lab <- function(poblacion, operacion = "CADR1L", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
 
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo)
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process() %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  # agrego indicador causas a despacho por mes mediante consulta directa base de datos para evitar filtrado de casos en zzz
  if(str_detect(poblacion$organismo[[1]], "jdo")) {
    cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
      apgyeDSL::interval(start_date, end_date) %>% 
      filter(iep %in% poblacion$organismo) %>% 
      mutate(fdesp = dmy(fdesp)) %>% 
      mutate(mes = date_part('month', fdesp)) %>% 
      group_by(iep, mes) %>% 
      summarise(causas_adespacho = n()) %>% 
      collect() %>% 
      filter(mes >= lubridate::month(start_date), 
             mes < lubridate::month(end_date)) %>% 
      mutate(mes = lubridate::month(mes, label = T, abbr = F))
    
    resultado <- resultado %>% 
      select(-causas_adespacho) %>% 
      left_join(cadesp, by = c("iep", "mes"))
    
    resultado
  } else {
    
    cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
      apgyeDSL::interval(start_date, end_date) %>% 
      filter(iep %in% poblacion$organismo) %>% 
      mutate(fdesp1 = dmy(fdesp1)) %>% 
      mutate(mes = date_part('month', fdesp1)) %>% 
      group_by(iep, mes) %>% 
      summarise(causas_adespacho = n()) %>% 
      collect() %>% 
      filter(mes >= lubridate::month(start_date), 
             mes < lubridate::month(end_date)) %>% 
      mutate(mes = lubridate::month(mes, label = T, abbr = F))
    
    resultado <- resultado %>% 
      select(-causas_adespacho) %>% 
      left_join(cadesp, by = c("iep", "mes"))
    
    resultado
    
  }
  
  if(str_detect(poblacion$organismo, "sal")) { # normalizacion nombre variable de sala
    resultado <- resultado %>% 
      rowwise() %>% 
      mutate(res_luego1venc = sum(res_luego1venc_crecirc, res_luego1venc_srecirc, na.rm = T)) %>% 
      rowwise()
  } 
  
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luego1venc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xindeter, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego1venc", replacement="luego_1er_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>%
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE))
      resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) %>% 
      janitor::adorn_totals("row") 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round(luego_1er_venc/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego 1er Vencim" = luego_1er_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha"))
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
# Familia
resoluciones_fam <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    filter(!grepl("VIOLENCIA", tproc)) 
  
  
  resultado <- resolverconvertidos(resultado)
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process() %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fdesp = dmy(fdesp)) %>% 
    mutate(mes = date_part('month', fdesp)) %>% 
    filter(!grepl("VIOLENCIA", tproc)) %>% 
    group_by(iep, mes) %>% 
    summarise(causas_adespacho = n()) %>% 
    collect() %>% 
    filter(mes >= lubridate::month(start_date), 
           mes < lubridate::month(end_date)) %>% 
    mutate(mes = lubridate::month(mes, label = T, abbr = F))
  
  resultado <- resultado %>% 
    select(-causas_adespacho) %>% 
    left_join(cadesp, by = c("iep", "mes"))
  
  
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luego1venc, res_luego2venc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xindeter, matches("fecha") ) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego1venc", replacement="luego_1er_venc") %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego2venc", replacement="luego_2do_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>% 
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE))
      resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) %>% 
      janitor::adorn_totals("row") 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_1er_venc + luego_2do_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego 1er Vencim" = luego_1er_venc,
           "luego 2do Vencim" = luego_2do_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha"))
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
# Paz
resoluciones_paz <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    filter(!grepl("VIOLENCIA", tproc)) 
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process() %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  cadesp <- DB_PROD() %>% apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fdesp = dmy(fdesp)) %>% 
    mutate(mes = date_part('month', fdesp)) %>% 
    filter(!grepl("VIOLENCIA", tproc)) %>% 
    group_by(iep, mes) %>% 
    summarise(causas_adespacho = n()) %>% 
    collect() %>% 
    filter(mes >= lubridate::month(start_date), 
           mes < lubridate::month(end_date)) %>% 
    mutate(mes = lubridate::month(mes, label = T, abbr = F))
  
  resultado <- resultado %>% 
    select(-causas_adespacho) %>% 
    left_join(cadesp, by = c("iep", "mes"))
  
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luego1venc, res_luego2venc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xindeter, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego1venc", replacement="luego_1er_venc") %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego2venc", replacement="luego_2do_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>% 
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE))
      resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_1er_venc + luego_2do_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego 1er Vencim" = luego_1er_venc,
           "luego 2do Vencim" = luego_2do_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha"))
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
resoluciones_paz_23 <- function(poblacion, start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(CADRA23) %>% 
    mutate(fres = dmy(fres)) %>% 
    mutate(fdesp = dmy(fdesp)) %>%
    mutate(fvenc1 = dmy(fvenc1)) %>%
    mutate(fvenc2 = dmy(fvenc2)) %>%
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    filter(!grepl("VIOLENCIA|CERTIFICACION|PROBATION|DILIGENCIAS|INFORMACION SUMARIA|SU ", tproc)) 
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process(CADRA23) %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F),
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  
  cadesp <- DB_PROD() %>% apgyeTableData(CADRA23) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fdesp = dmy(fdesp)) %>% 
    mutate(mes = date_part('month', fdesp)) %>% 
    filter(!grepl("VIOLENCIA|CERTIFICACION|PROBATION|DILIGENCIAS|INFORMACION SUMARIA|SU ", tproc)) %>%
    group_by(iep, mes) %>% 
    summarise(causas_adespacho = n()) %>% 
    collect() %>% 
    filter(mes >= lubridate::month(start_date), 
           mes < lubridate::month(end_date)) %>% 
    mutate(mes = lubridate::month(mes, label = T, abbr = F))
  
  resultado <- resultado %>% 
    select(-causas_adespacho) %>% 
    left_join(cadesp, by = c("iep", "mes"))
  
  
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luego1venc, res_luego2venc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xindeter, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego1venc", replacement="luego_1er_venc") %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luego2venc", replacement="luego_2do_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>% 
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE))
      resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_1er_venc + luego_2do_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego 1er Vencim" = luego_1er_venc,
           "luego 2do Vencim" = luego_2do_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha"))
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
# Contencioso Administrativo
resoluciones_cad_1 <- function(poblacion, operacion = "CADRCAD", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
 
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    #apgyeTableData(!! operacion) %>% 
    apgyeTableData(CADRCAD) %>%
    mutate(fres = dmy(fres)) %>% 
    mutate(fdesp = dmy(fdesp)) %>%
    mutate(fvenc = dmy(fvenc)) %>%
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo)
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process(CADRCAD) %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  cadesp <- DB_PROD() %>% apgyeTableData(CADRCAD) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fdesp = dmy(fdesp)) %>% 
    mutate(mes = date_part('month', fdesp)) %>% 
    group_by(iep, mes) %>% 
    summarise(causas_adespacho = n()) %>% 
    collect() %>% 
    filter(mes >= lubridate::month(start_date), 
           mes < lubridate::month(end_date)) %>% 
    mutate(mes = lubridate::month(mes, label = T, abbr = F))
  
  resultado <- resultado %>% 
    select(-causas_adespacho) %>% 
    left_join(cadesp, by = c("iep", "mes"))
 
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luegovenc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xotra, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luegovenc", replacement="luego_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>% 
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE))
      
        resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego Vencim" = luego_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas",
           matches("fecha"))
  
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
}
resoluciones_cad_stj <- function(poblacion, operacion = "CADRCADS", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(CADRCADS) %>%
    apgyeDSL::interval(start_date, end_date) %>%
    mutate(fres = dmy(fres)) %>% 
    mutate(fdesp = dmy(fdesp)) %>%
    mutate(fvenc = dmy(fvenc)) %>%
    filter(iep %in% poblacion$organismo)
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%  group_by(iep, data_interval_start, data_interval_end) 
  } else {
    resultado <- resultado %>% group_by(iep) 
  }
  
  resultado <- resultado %>% 
    process(CADRCADS) %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% 
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1)  
  }
  
  
  cadesp <- DB_PROD() %>% apgyeTableData(CADRCADS) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fdesp = dmy(fdesp)) %>% 
    mutate(mes = date_part('month', fdesp)) %>% 
    group_by(iep, mes) %>% 
    summarise(causas_adespacho = n()) %>% 
    collect() %>% 
    filter(mes >= lubridate::month(start_date), 
           mes < lubridate::month(end_date)) %>% 
    mutate(mes = lubridate::month(mes, label = T, abbr = F))
  
  resultado <- resultado %>% 
    select(-causas_adespacho) %>% 
    left_join(cadesp, by = c("iep", "mes"))
  
  
  resultado <- resultado %>% 
    select(organismo = iep, matches("mes"), causas_adespacho, causas_resueltas, 
           a_termino = res_atermino, res_luegovenc, sentencias = res_xsentencia, 
           autos = res_xauto, sin_clasif =  res_xotra, matches("fecha")) %>% 
    rename_all(.funs = stringr::str_replace_all, pattern = "res_luegovenc", replacement="luego_venc") %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = "organismo")
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo_descripcion) %>% 
        janitor::adorn_totals("row") 
      
      resultado
      
    } else {
      resultado <- resultado %>% 
        arrange(circunscripcion, organismo, mes) %>% 
        group_by(circunscripcion, organismo) %>% 
        do(janitor::adorn_totals(.) %>%
             mutate(mes=ifelse(organismo_descripcion=="-", "Total", mes), circunscripcion=.$circunscripcion[1], organismo_descripcion=.$organismo_descripcion[1]) ) %>% 
        ungroup() %>% 
        select(circunscripcion, organismo, mes, everything()) %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE)) 
        resultado
    }
    
  } else {
    resultado <- resultado %>% arrange(circunscripcion, organismo) 
  }
  resultado <- resultado %>% 
    select(circunscripcion, organismo, organismo_descripcion, everything()) %>% 
    filter(causas_resueltas > 0) %>% 
    mutate(circunscripcion = as.character(circunscripcion)) %>% 
    mutate(resol_vencidas = stringr::str_c(round((luego_venc)/causas_resueltas*100), ' %')) %>% 
    select("circunscripción" = circunscripcion,
           organismo=organismo_descripcion,
           matches("mes"),
           "causas a despacho del mes" = causas_adespacho,
           "causas resueltas" = causas_resueltas,
           "a termino" = a_termino,
           "luego Vencim" = luego_venc,
           sentencias,
           autos,
           "sin clasif" = sin_clasif,
           "resol vencidas" = "resol_vencidas", 
           matches("fecha"))
  
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
  
}
# CEMARC
resoluciones_cen <- function(poblacion, operacion = "CRMED", start_date = "2019-02-01", end_date = "2019-03-02", desagregacion_mensual = TRUE, cfecha = F) {
  
  un_mes <- ((lubridate::month(end_date) - lubridate::month(start_date)) == 1)
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!!operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% !!poblacion$organismo)
  
  
  if(desagregacion_mensual) {
    resultado <- resultado %>% group_by(iep, data_interval_start, data_interval_end)
  } else {
    resultado <- resultado %>% group_by(iep)
  }
  
  resultado <- resultado %>% 
    process() %>%
    .$result %>%
    ungroup()
  
  if(desagregacion_mensual) {
    resultado <- resultado %>%
      mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F), 
             fecha =  as.Date(data_interval_end) - 1) %>% 
      mutate(mediana_durac_inic_fin = as.character(mediana_durac_inic_fin)) %>% 
      select(tipo_circ, mes, tres_disc, resultado, mediana_durac_inic_fin, fecha)
  } else {
    resultado <- resultado %>%
      mutate(mediana_durac_inic_fin = as.character(mediana_durac_inic_fin)) %>% 
      select(tipo_circ, tres_disc, resultado, mediana_durac_inic_fin)
  }
  
  
  if(desagregacion_mensual) {
    if(un_mes) {
      resultado <- resultado %>% 
        rename(circunscripcion = tipo_circ, cantidad_mediaciones = resultado) %>% 
        select(-mediana_durac_inic_fin) %>% 
        mutate(mes = as.character(mes)) %>% 
        tidyr::spread(tres_disc, cantidad_mediaciones, drop = T, fill = 0) %>% 
        janitor::adorn_totals("col") %>% 
        janitor::adorn_totals("row")
        
      resultado
      
    } else {
      
      resultado <- resultado %>% 
        rename(circunscripcion = tipo_circ, cantidad_mediaciones = resultado) %>% 
        select(-mediana_durac_inic_fin) %>% 
        mutate(mes = as.character(mes)) %>% 
        tidyr::spread(tres_disc, cantidad_mediaciones, drop = T, fill = 0 ) %>% 
        janitor::adorn_totals("col") %>% 
        mutate(mes = factor(mes, levels = c("enero", "febrero", "marzo", "abril", "mayo", 
                                            "junio", "julio", "agosto", "septiembre", 
                                            "octubre", "noviembre", "diciembre", "Total"),
                            ordered = TRUE)) %>%
        arrange(circunscripcion, mes) %>% 
        group_by(circunscripcion) %>% 
        do(janitor::adorn_totals(.))
      
      resultado
      
    }
    
  } else {
    resultado <- resultado %>% 
      rename(circunscripcion = tipo_circ, cantidad_mediaciones = resultado) %>% 
      select(-mediana_durac_inic_fin) %>%
      tidyr::spread(tres_disc, cantidad_mediaciones, drop = T, fill = 0) %>% 
      janitor::adorn_totals("col") %>%
      janitor::adorn_totals("row")
    
    resultado
    
  }
  
  if(desagregacion_mensual) { if(!cfecha) {resultado <- resultado %>% select(-fecha)} }
  
  resultado
  
}


## Resoluciones por Tipo 
resoluciones_cco_xtres <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE) {
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData("CADR1C") %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fres = dmy(fres)) %>%  mutate(fdesp = dmy(fdesp)) %>% 
    mutate(finicio = dmy(finicio)) %>% mutate(fvenc1 = dmy(fvenc1)) %>% 
    filter(!is.na(fres)) %>% 
    filter(toupper(as) == "S") %>% 
    collect() %>% 
    filter(fres >= data_interval_start & fres < data_interval_end) %>% 
    filter(!tres %in% c("0")) %>% 
    ungroup() %>% 
    mutate(tipo_resolucion = case_when(
      tres == "1" ~ "Conciliación",
      tres == "2" ~ "Transacción", 
      tres == "3" ~ "Caducidad",
      tres == "4" ~ "Desistimiento",
      tres == "5" ~ "Allanamiento",
      tres == "6" ~ "Incompetencia",
      tres == "7" ~ "Excepción",
      tres == "8" ~ "Sentencia monitoria sin oposición",
      tres == "9" ~ "Sentencia monitoria con oposición",
      tres == "10" ~ "Sent.Definitiva", # en código otra se registraba sent.def
      tres == "11" ~ "Rechazo “In Limine”",
      #Códigos Específicos para resoluciones en Procesos Sucesorios (a partir del 01/11/18)
      tres == "31" ~ "Sucesorio:Declarat.Herederos",
      tres == "32" ~ "Sucesorio:Ampliac./Rect.Declarat.Her.",
      tres == "33" ~ "Sucesorio:Aprobac.Testamento",
      tres == "34" ~ "Sucesorio:Inventario Avalúo",
      tres == "35" ~ "Sucesorio:Partic.yAdjudic.",
      tres == "36" ~ "Sucesorio:Herencia Vacante",
      tres == "37" ~ "Sucesorio:otras resoluciones",
      TRUE ~ "Sent.Definitiva")) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = c("iep" = "organismo")) %>% 
    group_by(circunscripcion, organismo_descripcion, tipo_resolucion) %>% 
    summarise(cantidad = n()) %>% 
    #filter(!tipo_resolucion %in% c( "Incompetencia", "Excepción")) %>%
    do(janitor::adorn_totals(.)) %>% 
    ungroup() 
  
  resultado
  
}
resoluciones_ecq_xtres <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02") {
  
  operacion = rlang::enexpr(operacion)

  prim <- DB_PROD() %>%
    apgyeTableData("CADR1C") %>%
    apgyeDSL::interval(start_date, end_date) %>%
    filter(iep %in% poblacion$organismo) %>%
    mutate(fres = dmy(fres)) %>%  mutate(fdesp = dmy(fdesp)) %>%
    mutate(finicio = dmy(finicio)) %>% mutate(fvenc1 = dmy(fvenc1)) %>%
    filter(!is.na(fres)) %>%
    mutate(as = toupper(as)) %>% 
    filter(as %in% c("S", "S1", "S2", "S3", "S4", "S5", "S6", "S7")) %>%
    collect() %>%
    filter(fres >= data_interval_start & fres < data_interval_end) %>%
    filter(!tres %in% c("0")) %>%
    ungroup() %>%
    mutate(tres = case_when(
      tres == "1" ~ "Conciliación",
      tres == "2" ~ "Transacción",
      tres == "3" ~ "Caducidad",
      tres == "4" ~ "Desistimiento",
      tres == "5" ~ "Allanamiento",
      tres == "6" ~ "Incompetencia",
      tres == "7" ~ "Excepción",
      tres == "8" ~ "Sentencia monitoria sin oposición",
      tres == "9" ~ "Sentencia monitoria con oposición",
      tres == "10" ~ "Sentencia", # en código otra se registraba sent.def
      tres == "11" ~ "Rechazo “In Limine”",
      #Códigos Específicos para resoluciones en Procesos Sucesorios (a partir del 01/11/18)
      tres == "31" ~ "Sucesorio:Declarat.Herederos",
      tres == "32" ~ "Sucesorio:Ampliac./Rect.Declarat.Her.",
      tres == "33" ~ "Sucesorio:Aprobac.Testamento",
      tres == "34" ~ "Sucesorio:Inventario Avalúo",
      tres == "35" ~ "Sucesorio:Partic.yAdjudic.",
      tres == "36" ~ "Sucesorio:Herencia Vacante",
      tres == "37" ~ "Sucesorio:otras resoluciones",
      TRUE ~ "Sentencia")) %>% 
    mutate(tres = ifelse(as == "S1", "Sent_Conc_Preventivo", tres)) %>% 
    mutate(tres = ifelse(as == "S2", "Sent_Conversion_Quieb_en_Conc", tres)) %>% 
    mutate(tres = ifelse(as == "S3", "Sent_Quiebra", tres)) %>% 
    mutate(tres = ifelse(as == "S4", "Sent_Verific_hasta_20_verif", tres)) %>% 
    mutate(tres = ifelse(as == "S5", "Sent_Verific_más_de_20_verif", tres)) %>% 
    mutate(tres = ifelse(as == "S6", "Sent_Homologación", tres)) %>% 
    mutate(tres = ifelse(as == "S7", "Otras", tres)) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion",
                                                              "circunscripcion")],
              by = c("iep" = "organismo")) 
  
  df_resoluciones <- prim %>% 
    group_by(circunscripcion, organismo_descripcion, tipo_sentencia = tres) %>%
    summarise(cantidad = n()) %>%
    arrange(circunscripcion, organismo_descripcion, desc(cantidad)) %>% 
    #filter(!tipo_resolucion %in% c( "Incompetencia", "Excepción")) %>%
    do(janitor::adorn_totals(.)) %>%
    ungroup() 
  
  df_resoluciones
  
}
resoluciones_lab_xtres <- function(poblacion, operacion = "CADR1L", start_date = "2018-07-01", end_date = "2018-09-02", desagregacion_mensual = TRUE) {
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData("CADR1L") %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fres = dmy(fres)) %>%  mutate(fdesp = dmy(fdesp)) %>% 
    mutate(finicio = dmy(finicio)) %>% mutate(fvenc = dmy(fvenc)) %>% 
    filter(!is.na(fres)) %>% 
    filter(toupper(as) == "S") %>% 
    collect() %>% 
    filter(fres >= data_interval_start & fres < data_interval_end) %>% 
    filter(!tres %in% c("0")) %>% 
    ungroup() %>% 
    mutate(tipo_resolucion = case_when(
      tres == "1" ~ "Conciliación",
      tres == "2" ~ "Transacción", 
      tres == "3" ~ "Caducidad",
      tres == "4" ~ "Desistimiento",
      tres == "5" ~ "Allanamiento",
      tres == "6" ~ "Incompetencia",
      tres == "7" ~ "Excepciones Previas",
      tres == "8" ~ "Otra",
      tres == "9" ~ "Rechazo “In Limine”",
      TRUE ~ "Sent.Definitiva")) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = c("iep" = "organismo")) %>% 
    group_by(circunscripcion, organismo_descripcion, tipo_resolucion) %>% 
    summarise(cantidad = n()) %>% 
    #filter(!tipo_resolucion %in% c( "Incompetencia", "Excepción")) %>%
    do(janitor::adorn_totals(.)) %>% 
    ungroup() 
  
  resultado
  
}
resoluciones_cco3_xtres <- function(poblacion, start_date = "2018-07-01", end_date = "2018-09-02") {
  
  resultado <- DB_PROD() %>% 
    apgyeTableData("CADR3C") %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fres = dmy(fres)) %>%  
    filter(!is.na(fres)) %>% 
    filter(toupper(as) == "S") %>% 
    collect() %>% 
    filter(fres >= data_interval_start & fres < data_interval_end) %>% 
    filter(tres != "0") %>% 
    ungroup() %>% 
    mutate(tipo_resolucion = case_when(
      tres == "1" ~ "RIL Admisible",
      tres == "2" ~ "RIL Inadmisible", 
      tres == "3" ~ "RIL Procedente",
      tres == "4" ~ "RIL Improcedente",
      tres == "5" ~ "Acepta Rec. De Queja",
      tres == "6" ~ "Rechaza Rec.de Queja",
      tres == "7" ~ "Concesión Rec. Extraordinario",
      tres == "8" ~ "Denegación Rec. Extraordinario",
      tres == "9" ~ "Regulación de Honorarios",
      tres == "10" ~ "OTRAS",
      TRUE ~ "sin dato")) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = c("iep" = "organismo")) %>% 
    group_by(circunscripcion, organismo_descripcion, tipo_resolucion) %>% 
    summarise(cantidad = n()) %>% 
    #filter(!tipo_resolucion %in% c( "Incompetencia", "Excepción")) %>%
    do(janitor::adorn_totals(.)) %>% 
    ungroup() 
  
  resultado
  
}
resoluciones_lab3_xtres <- function(poblacion, start_date = "2018-07-01", end_date = "2018-09-02") {
  
  resultado <- DB_PROD() %>% 
    apgyeTableData("CADR3L") %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    mutate(fres = dmy(fres)) %>%  
    filter(!is.na(fres)) %>% 
    filter(toupper(as) == "S") %>% 
    collect() %>% 
    filter(fres >= data_interval_start & fres < data_interval_end) %>% 
    filter(tres != "0") %>% 
    ungroup() %>% 
    mutate(tipo_resolucion = case_when(
      tres == "1" ~ "Declara Mal Concedido RIL",
      tres == "2" ~ "RIL Inadmisible", 
      tres == "3" ~ "RIL Procedente",
      tres == "4" ~ "RIL Improcedente",
      tres == "5" ~ "Hace lugar Rec. De Queja",
      tres == "6" ~ "Rechaza Rec.de Queja",
      tres == "7" ~ "Concesión Rec. Extraordinario",
      tres == "8" ~ "Denegación Rec. Extraordinario",
      tres == "9" ~ "Regulación de Honorarios",
      tres == "10" ~ "Otras",
      TRUE ~ "sin dato")) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], 
              by = c("iep" = "organismo")) %>% 
    group_by(circunscripcion, organismo_descripcion, tipo_resolucion) %>% 
    summarise(cantidad = n()) %>% 
    #filter(!tipo_resolucion %in% c( "Incompetencia", "Excepción")) %>%
    do(janitor::adorn_totals(.)) %>% 
    ungroup() 
  
  resultado
  
}

# Resoluciones Violencia: Familia y Paz

movimientos_violencias <- function(poblacion, start_date, end_date) {
  
  #Retorna la cantidad de movimientos Ley 9198 por jurisdicción y juzgado, según la fecha de los mismos, 
  # considerando las nuevas denuncias, medidas y resoluciones/sentencias.
  
  # Retorna la cantidad de movimientos Ley 10058 por jurisdicción y juzgado, según la fecha de los mismos, 
  # considerando las nuevas denuncias, medidas y resoluciones/sentencias.
  
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  inicio <- paste0("'", strftime(start_date, "%d-%m-%Y"), "'")
  
  end_date <- as.Date(end_date, format = "%Y-%m-%d") - 1
  fin <- paste0("'", strftime(end_date, "%d-%m-%Y"), "'")
  
  query1 <- paste("select * from movimientos_9198(",inicio,",",fin,")")
  
  result1 <- DB_REJUCAV() %>% 
    dbGetQuery(query1) %>% 
    mutate(movimientos_tipo = "violencia_familiar")
  
  query2 <- paste("select * from movimientos_10058(",inicio,",",fin,")")
  
  result2 <-  DB_REJUCAV() %>% 
    dbGetQuery(query2) %>% 
    mutate(movimientos_tipo = "violencia_c-mujer")
  
  result <- result1 %>% 
    bind_rows(result2) %>% 
    tidyr::spread(key = movimientos_tipo, value = cantidad, fill = 0)
  
  fuero <- ifelse(any(str_detect(poblacion$organismo, "fam")), "fam", "paz")
  
  result <- result %>% 
    filter(str_detect(tolower(juzgado), fuero)) %>% 
    filter(!str_detect(tolower(juzgado), "defensor")) %>% 
    select(jurisdiccion, juzgado, everything()) %>% 
    mutate(juzgado = str_to_title(juzgado), jurisdiccion = str_to_title(jurisdiccion)) %>% 
    arrange(jurisdiccion, juzgado ) %>% 
    janitor::adorn_totals("col")
  
  result
  
}


# Acceso a primarias
resoluciones_cco_prim <- function(poblacion, operacion = "CADR1C", start_date = "2018-07-01", end_date = "2018-09-02") {
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    resolverconvertidos() %>% 
    mutate(fres = dmy(fres), fvenc1 = dmy(fvenc1), fvenc2 = dmy(fvenc2)) %>% 
    collect() %>% 
    mutate(med_mej_prov = ifelse(tres == "0", 1, NA))
  
  if(any(str_detect(colnames(resultado), "fdesp1"))) {
    
    resultado <- resultado %>% 
      mutate(fdesp = as.Date(fdesp1, format="%d/%m/%Y"))
      
    resultado
  }
  
   
  if(str_detect(poblacion$organismo, "sal")) { # normalizacion nombre variable de sala
    resultado <- resultado %>%
      filter(!is.na(fres)) %>% 
      mutate(control_intervalo =  fres >= data_interval_start & fres < data_interval_end) %>% 
      mutate(control_mmp = tres != 0 | is.na(tres)) %>% 
      mutate(
        atermino = 
          (fres <= fvenc1 & 
             control_intervalo & 
             control_mmp), 
        res_luego1venc = 
          (fres > fvenc1 &
             control_intervalo &
             control_mmp), 
        res_luego2venc = NA)
    resultado
    
  } else {
    resultado <- resultado %>%
      filter(!is.na(fres)) %>% 
      mutate(control_intervalo =  fres >= data_interval_start & fres < data_interval_end) %>% 
      mutate(control_mmp = tres != 0 | is.na(tres)) %>% 
      mutate(
        atermino = 
          (fres <= fvenc1 & 
             control_intervalo & 
             control_mmp), 
        res_luego1venc = 
          (fres > fvenc1 &
             fres <= fvenc2 & 
             control_intervalo &
             control_mmp), 
        res_luego2venc = 
          (fres > fvenc2 &
             control_intervalo &
             control_mmp))
    resultado
    
  }
  
  
  resultado <- resultado %>% 
    rowwise() %>% 
    mutate(inconsistencia = sum(atermino, res_luego1venc, res_luego2venc, na.rm = T)) %>% 
    rowwise() %>% 
    mutate(atermino = ifelse(atermino, 1, NA),
           res_luego1venc = ifelse(res_luego1venc, 1, NA), 
           res_luego2venc = ifelse(res_luego2venc, 1, NA), 
           inconsistencia = ifelse(inconsistencia == 2, 1, NA)) %>% 
    mutate(caratula = str_sub(caratula, 1,10)) %>% 
    mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F)) %>% 
    select(organismo = iep, mes, nro, caratula, fecha_despacho = fdesp, fvenc1, fvenc2,  fecha_resolucion = fres, 
           med_mej_prov, atermino, res_luego1venc, res_luego2venc, inconsistencia2venc = inconsistencia) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], by = "organismo") %>% 
    select(circunscripcion, organismo = organismo_descripcion, everything(), - organismo) %>% 
    arrange(circunscripcion, organismo, mes) %>% 
     mutate(#circunscripcion = abbreviate(circunscripcion, minlength = 4),
            organismo = abbreviate(organismo, minlength = 13)) %>% 
    select(-caratula) %>% 
    janitor::adorn_totals("row")
  
  
  resultado
  
}
resoluciones_lab_prim <- function(poblacion, operacion = "CADR1L", start_date = "2018-07-01", end_date = "2018-09-02") {
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    resolverconvertidos() %>% 
    mutate(fres = dmy(fres), fvenc = dmy(fvenc)) %>% 
    collect() %>% 
    mutate(med_mej_prov = ifelse(tres == "0", 1, NA))
  
  
  if(any(str_detect(colnames(resultado), "fdesp1"))) {
    
    resultado <- resultado %>% 
      mutate(fdesp = as.Date(fdesp1, format="%d/%m/%Y"))
    
    resultado
  }
  
  
  resultado <- resultado %>%
    filter(!is.na(fres)) %>% 
    mutate(control_intervalo =  fres >= data_interval_start & fres < data_interval_end) %>% 
    mutate(control_mmp = tres != 0 | is.na(tres)) %>% 
    mutate(
      atermino = 
        (fres <= fvenc & 
           control_intervalo & 
           control_mmp), 
      res_luego1venc = 
        (fres > fvenc &
           control_intervalo &
           control_mmp)) %>% 
    rowwise() %>% 
    mutate(inconsistencia = sum(atermino, res_luego1venc, na.rm = T)) %>% 
    rowwise() %>% 
    mutate(atermino = ifelse(atermino, 1, NA),
           res_luego1venc = ifelse(res_luego1venc, 1, NA), 
           inconsistencia = ifelse(inconsistencia == 2, 1, NA)) %>% 
    mutate(caratula = str_sub(caratula, 1,10)) %>% 
    mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F)) %>% 
    select(organismo = iep, mes, nro, caratula, fecha_despacho = fdesp, fecha_resolucion = fres,  med_mej_prov,  atermino, res_luego1venc, 
           inconsistencia2venc = inconsistencia) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], by = "organismo") %>% 
    select(circunscripcion, organismo = organismo_descripcion, everything(), - organismo) %>% 
    arrange(circunscripcion, organismo, mes) %>% 
    # mutate(circunscripcion = abbreviate(circunscripcion, minlength = 4),
    #        organismo = abbreviate(organismo, minlength = 10)) %>% 
    select(-caratula) %>% 
    janitor::adorn_totals("row")
  
  
  resultado
  
}
resoluciones_cad_prim <- function(poblacion, operacion = "CADRCAD", start_date = "2018-07-01", end_date = "2018-09-02") {
  
  operacion = rlang::enexpr(operacion)
  
  resultado <- DB_PROD() %>% 
    apgyeTableData(!! operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep %in% poblacion$organismo) %>% 
    resolverconvertidos() %>% 
    mutate(fdesp = dmy(fdesp), fres = dmy(fres), fvenc = dmy(fvenc)) %>% 
    collect() %>% 
    mutate(med_mej_prov = ifelse(tres == "0", 1, NA))
  
  
  resultado <- resultado %>%
    filter(!is.na(fres)) %>% 
    mutate(control_intervalo =  fres >= data_interval_start & fres < data_interval_end) %>% 
    mutate(control_mmp = tres != 0 | is.na(tres)) %>% 
    mutate(
      atermino = 
        (fres <= fvenc & 
           control_intervalo & 
           control_mmp), 
      res_luego1venc = 
        (fres > fvenc &
           control_intervalo &
           control_mmp)) %>% 
    rowwise() %>% 
    mutate(inconsistencia = sum(atermino, res_luego1venc, na.rm = T)) %>% 
    rowwise() %>% 
    mutate(atermino = ifelse(atermino, 1, NA),
           res_luego1venc = ifelse(res_luego1venc, 1, NA), 
           inconsistencia = ifelse(inconsistencia == 2, 1, NA)) %>% 
    mutate(caratula = str_sub(caratula, 1,10)) %>% 
    mutate(mes = lubridate::month(data_interval_start, label=T, abbr = F)) %>% 
    select(organismo = iep, mes, nro, caratula, fecha_despacho = fdesp, fvenc, fecha_resolucion = fres, med_mej_prov, atermino, res_luego1venc, 
           inconsistencia2venc = inconsistencia) %>% 
    left_join(apgyeJusEROrganization::listar_organismos()[, c("organismo", "organismo_descripcion", 
                                                              "circunscripcion")], by = "organismo") %>% 
    select(circunscripcion, organismo = organismo_descripcion, everything(), - organismo) %>% 
    arrange(circunscripcion, organismo, mes) %>% 
    mutate(#circunscripcion = abbreviate(circunscripcion, minlength = 4),
            organismo = abbreviate(organismo, minlength = 10)) %>% 
    select(-caratula) %>% 
    janitor::adorn_totals("row")
  
  
  resultado
  
}



