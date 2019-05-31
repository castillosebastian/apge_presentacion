newenv <- function(...) {
  envinitialization <- rlang::enexprs(...)
  env = new.env()
  for(var in names(envinitialization)) {
    env[[var]] = eval(envinitialization[[var]])
  }
  env
}

outputTable <- function (table, caption, row_group_label_position = "identity") {
  if(row_group_label_position == "identity") {
    total_rows <-  c(which(table[, 1] == "Total"), which(table[, 2] == "Total"), which(table[, 3] == "Total"))
  } else {
    total_rows <-  c(which(table[, 2] == "Total"), which(table[, 3] == "Total"))
  }
  table %>% rename_all(.funs = stringr::str_replace_all, pattern = "_", replacement=" ") %>% 
    kable("latex", caption = caption,
          align = 'c', longtable = TRUE, booktabs = T ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), latex_options = c("repeat_header"),
                  full_width = F, font_size = 10) %>% 
    row_spec(total_rows, bold = T) %>% 
    collapse_rows(columns = 1:2, row_group_label_position = row_group_label_position)
  
}

getDataIntervalStr <- function(data_interval_start, data_interval_end){
  year <- lubridate::year(data_interval_start);
  year_end <- lubridate::year(data_interval_end);
  month <- lubridate::month(data_interval_start);
  month_end <- lubridate::month(lubridate::as_date(data_interval_end)  - lubridate::days(1));
  monthDiff <- lubridate::interval(data_interval_start %>% as.Date(), data_interval_end %>% as.Date()) %/% months(1)
  period <- format(lubridate::make_date(year, month, 1), '%B %Y')
  
  period[monthDiff == 12] <- paste("anual", year[monthDiff == 12])
  period[monthDiff < 12] <- paste(format(lubridate::make_date(year, month, 1), '%B'), '-', format(lubridate::make_date(year, month_end, 1), '%B'), year[monthDiff < 12])
  period[monthDiff > 12] <- paste(format(lubridate::make_date(year, month, 1), '%B %Y'), '-', format(lubridate::make_date(year_end, month_end, 1), '%B %Y'))
  
  period[monthDiff == 6 & month == 1] <- paste("primer semestre", year[monthDiff == 6 & month == 1])
  period[monthDiff == 6 & month == 7] <- paste("segundo semestre", year[monthDiff == 6 & month == 7])
  period[monthDiff == 1] <- paste(format(lubridate::make_date(year[monthDiff == 1], month[monthDiff == 1], 1), '%B %Y'))
  
  
  period
}

resolverconvertidos <- function(df) {
  df <- df %>% 
    mutate(iep = ifelse(iep == "jdofam0101con", "jdofam0100con", iep)) %>% 
    mutate(iep = ifelse(iep == "jdofam0102con", "jdofam0300con", iep)) %>% 
    mutate(iep = ifelse(iep == "jdofam0201con", "jdofam0200con", iep)) %>% 
    mutate(iep = ifelse(iep == "jdofam0202con", "jdofam0300con", iep)) %>% 
    mutate(iep = ifelse(iep == "jdofam0001gch", "jdofam0100gch", iep)) %>%
    mutate(iep = ifelse(iep == "jdofam0002gch", "jdofam0200gch", iep)) %>%
    mutate(iep = ifelse(iep == "jdofam0001uru", "jdofam0200uru", iep)) %>%
    mutate(iep = ifelse(iep == "jdofam0002uru", "jdofam0200uru", iep))
  df
}

redistribuirxgpo <- function(dfinic, perdedores, ganadores, grupos, inversagrupo = FALSE) {
  
  #dfinic toma salida de iniciados_cco(jdos_cco,start_date, end_date)
  #perdedores pierden grupo
  #inversa de grupo significa todos menos el grupo
  
  dfinic <- dfinic %>% 
    filter(!tipo_proceso == "subtotal")
  
  if(diffgrupo){
    casos_aredistribuir <- dfinic %>% 
      filter(organismo %in% perdedores$organismo_descripcion) %>% 
      filter(!grupo_proceso %in% grupos) 
    casos_aredistribuir
  } else {
    casos_aredistribuir <- dfinic %>% 
      filter(organismo %in% perdedores$organismo_descripcion) %>% 
      filter(grupo_proceso %in% grupos) 
    casos_aredistribuir
  }
  
  
  if(nrow(ganadores) == 1) {
    casos_redistribuidos <- casos_aredistribuir %>% 
      group_by(circunscripcion, grupo_proceso) %>% 
      summarise(tipo_proceso = "-", 
                organismo = ganadores$organismo_descripcion,
                cantidad = sum(cantidad, na.rm = T)) %>% 
      select(colnames(dfinic)) 
    casos_redistribuidos
  } else {
    casos_redistribuidos <- casos_aredistribuir %>% 
      group_by(circunscripcion, grupo_proceso) %>% 
      summarise(tipo_proceso = "-", 
                organismo = NA, 
                cantidad = round(sum(cantidad/nrow(ganadores), na.rm = T))) %>% 
      select(colnames(dfinic)) %>% 
      bind_rows(replicate(nrow(ganadores)-1, ., simplify = FALSE)) %>% 
      mutate(organismo = rep(ganadores$organismo_descripcion, 
                             each = length(unique(casos_aredistribuir$grupo_proceso))))
    casos_redistribuidos
  }
  
  dfredistribuidos <- dfinic %>% 
    anti_join(casos_aredistribuir, by = c("organismo", "grupo_proceso", "tipo_proceso")) %>% 
    bind_rows(casos_redistribuidos) %>% 
    group_by(circunscripcion, organismo, grupo_proceso) %>% 
    arrange(circunscripcion, organismo, grupo_proceso, desc(cantidad)) %>% 
    #do(janitor::adorn_totals(.)) %>% 
    ungroup()
  
  dfredistribuidos
  
}

agrupartproc <- function(df, materia) {
  if (materia ==  "civil") {
    df <- df %>% 
      mutate(tproc = toupper(tproc)) %>%
      mutate(gtproc_forma = case_when(
        str_detect(tproc, "^ORDINARIO|^ORDINARIO |USUCAP") ~ "ORDINARIOS", #
        str_detect(tproc, "^SUMARISIMO|^SUMARISIMO ") ~ "SUMARISIMO",  #
        str_detect(tproc, "^INCIDENTE|^INCIDENTE ") ~ "INCIDENTES",
        str_detect(tproc, "^MEDIDA CAUTELAR|^MEDIDA CAUTELAR ") ~ "CAUTELARES",
        str_detect(tproc, "MONITORIO") ~ "MONITORIOS",
        str_detect(tproc, "^EJECU|^PROCESO DE EJECU|^APREMIO") ~ "EJECUCIONES", #
        str_detect(tproc, "^INTERDICT") ~ "INTERDICTOS",
        str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
        str_detect(tproc, "^PREPARACION ") ~ "PREPARATORIOS",
        str_detect(tproc, "^SUCESORIO") ~ "SUCESORIOS", #
        str_detect(tproc, "^BENEFICIO") ~ "BENEF.LITIG.SG.", #
        str_detect(tproc, "^HOMOLOGACION") ~ "HOMOLOGACIONES", 
        str_detect(tproc, "^DESALOJO") ~ "DESALOJO", 
        str_detect(tproc, "^COBRO") ~ "COBRO DE PESOS", #
        # Anomalías: desclasificados, no procesos y errores
        str_detect(tproc, "OFICIO|EXHORTO|SUMARIO|COMPENSACION|MATRICULA DE COMERCIANTE|DESARCHIVO|^INFORME|^DENUNCIA") ~ "noprocesos",
        str_detect(tproc, "CIVIL Y COMERCIAL|PROCESO LABORAL|PROCESO DE FAMILIA|CONCURSOS Y QUIEBRAS|^SENTENCIA|SOLICITA") ~ "desclasificados",
        str_detect(tproc, "CONCURSO CERRADO|EXPEDIENTE INTERNO|ADMINISTRATIVO|PERSONAL") ~ "error",
        # Otros Fueros Laboral y Familia
        str_detect(tproc, "LABORAL|TRABAJO|COMISION MEDICA|SUSTITUCION DE DEPOSITO|ENFERMEDAD PROFESIONAL|SALARIO|SINDICAL|CONCILIACION") ~ "desclasificados", 
        str_detect(tproc, "VIOLENCIA|DIVORCIO|ALIMENTO|RESTRICCIONES CAPAC|REGIMEN COMUNIC|^IMPEDIMENTO DE CON") ~ "desclasificados",
        str_detect(tproc, "AMENAZA|^MEDIDA DE PROTEC|GUARDA|INSCRIPCION|INTERNACION|RECONSTRUCCION DE EX|CAPACIDAD|TUTELA|SALUD MENTAL|^SU ") ~ "desclasificados",
        str_detect(tproc, "^ADOPCION|^IMPUGNACION DE RECON|^USO DE DOCUMENTO|^TESTIMONIO") ~ "desclasificados",
        TRUE ~ "OTROS"
      )) %>% 
      filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error"))
    df
  } else if(materia == "familia") {
    df <- df %>%
      mutate(tproc = toupper(tproc)) %>%
      mutate(gtproc_forma = case_when(
        str_detect(tproc, "VIOLENCIA FAMILIAR") ~ "VIOLENCIA FAMILIAR", #
        str_detect(tproc, "VIOLENCIA DE GENERO") ~ "VIOLENCIA DE GENERO", #
        str_detect(tproc, "HOMOLOGACION") ~ "HOMOLOGACIONES", 
        str_detect(tproc, "DIVORCIO") ~ "DIVORCIOS", 
        str_detect(tproc, "ALIMENTO") ~ "ALIMENTOS",  #
        str_detect(tproc, "^EJECU|^PROCESO DE EJECU") ~ "EJECUCIONES",
        str_detect(tproc, "^INCIDENTE|^INCIDENTE ") ~ "INCIDENTES",
        str_detect(tproc, "^MEDIDA") ~ "MEDIDAS",
        str_detect(tproc, "CAPACIDAD") ~ "CAPACIDAD/INCAPAC.",
        str_detect(tproc, "^ORDINARIO|^ORDINARIO ") ~ "ORDINARIOS", #
        str_detect(tproc, "^INTERNACION") ~ "INTERNACIONES", #
        str_detect(tproc, "^AUTORIZACION") ~ "AUTORIZACIONES", #
        str_detect(tproc, "^BENEFICIO") ~ "BENEF.LITIG.SG.", #
        str_detect(tproc, "^REGIMEN") ~ "REGIMEN COMUNIC.", #
        str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
        # Anomalías: desclasificados, no procesos y errores
        str_detect(tproc, "^SU |^EXHORTO|^OFICIO|DESARCHIVO|^INFORME|^DENUNCIA|^TESTIMONIO") ~ "noprocesos",
        str_detect(tproc, "CONCURSOS Y QUIEBRAS|^SENTENCIA|SOLICITA|SUCESORIO|USUCAPION") ~ "desclasificados",
        str_detect(tproc, "CONCURSO CERRADO|EXPEDIENTE INTERNO|ADMINISTRATIVO|PERSONAL") ~ "error",
        str_detect(tproc, "LABORAL|TRABAJO|COMISION MEDICA|SUSTITUCION DE DEPOSITO|ENFERMEDAD PROFESIONAL|SALARIO|SINDICAL|CONCILIACION") ~ "desclasificados",
        TRUE ~ "OTROS")) %>% 
      filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error")) 
    df
  } else if(materia == "laboral") {
    df <- df %>% 
      mutate(tproc = toupper(tproc)) %>%
      mutate(gtproc_forma = case_when(
        str_detect(tproc, "^COBRO") ~ "COBRO DE PESOS", #
        str_detect(tproc, "^ACCIDENTE") ~ "ACCIDENTE DE TRABAJO", #
        str_detect(tproc, "^APELACION DIC") ~ "APELACION COMISION MEDICA", #
        str_detect(tproc, "^HOMOLOGACION") ~ "HOMOLOGACIONES", #
        str_detect(tproc, "^EJEC|^PROCESO DE EJECU|^APREMIO") ~ "EJECUCIONES", #
        str_detect(tproc, "^ORDINARIO|^ORDINARIO ") ~ "ORDINARIOS", #
        str_detect(tproc, "^SUMARISIMO|^SUMARISIMO ") ~ "SUMARISIMO",  #
        str_detect(tproc, "^INCIDENTE|^INCIDENTE ") ~ "INCIDENTES",
        str_detect(tproc, "^MEDIDA CAUTELAR|^MEDIDA CAUTELAR ") ~ "CAUTELARES",
        str_detect(tproc, "^MEDIDAS PREP") ~ "MEDIDAS PREPARAT.",
        str_detect(tproc, "^BENEFICIO") ~ "BENEF.LITIG.SG.", #
        str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
        # Anomalías: desclasificados, no procesos y errores
        str_detect(tproc, "OFICIO|EXHORTO|SUMARIO|COMPENSACION|MATRICULA DE COMERCIANTE|DESARCHIVO|^INFORME|^DENUNCIA") ~ "noprocesos",
        str_detect(tproc, "CIVIL Y COMERCIAL|PROCESO DE FAMILIA|CONCURSOS Y QUIEBRAS|^SENTENCIA|SOLICITA|SUCESORIO|USUCAPION") ~ "desclasificados",
        str_detect(tproc, "CONCURSO CERRADO|EXPEDIENTE INTERNO|ADMINISTRATIVO|MONITORI|PERSONAL") ~ "error",
        str_detect(tproc, "VIOLENCIA|DIVORCIO|ALIMENTO|RESTRICCIONES CAPAC|REGIMEN COMUNIC|^IMPEDIMENTO DE CON") ~ "desclasificados",
        str_detect(tproc, "AMENAZA|^MEDIDA DE PROTEC|GUARDA|INSCRIPCION|INTERNACION|RECONSTRUCCION DE EX|CAPACIDAD|TUTELA|SALUD MENTAL|^SU ") ~ "desclasificados",
        str_detect(tproc, "^ADOPCION|^IMPUGNACION DE RECON|^USO DE DOCUMENTO|^TESTIMONIO|^DESALOJO|^APELACION JUZGADO|^DIVISION DE|^SEGUNDO TEST") ~ "desclasificados",
        TRUE ~ "OTROS")) %>% 
      filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error")) 
    df
  } else if(materia == "pazproc") {
      df <- df %>% 
        mutate(tproc = toupper(tproc)) %>%
        mutate(gtproc_forma = case_when(
          str_detect(tproc, "^ORDINARIO|^ORDINARIO |USUCAP") ~ "ORDINARIOS", #
          str_detect(tproc, "^SUMARISIMO|^SUMARISIMO ") ~ "SUMARISIMO",  #
          str_detect(tproc, "^COBRO DE PESOS") ~ "COBRO DE PESOS",  #
          str_detect(tproc, "^INCIDENTE|^INCIDENTE ") ~ "INCIDENTES", #
          str_detect(tproc, "^MEDIDA CAUTELAR|^MEDIDA CAUTELAR ") ~ "CAUTELARES", #
          str_detect(tproc, "MONITORIO") ~ "MONITORIOS", #
          str_detect(tproc, "^EJECU|^PROCESO DE EJECU|^APREMIO") ~ "EJECUCIONES", #
          str_detect(tproc, "^INTERDICT") ~ "INTERDICTOS",
          str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
          str_detect(tproc, "^PREPARACION ") ~ "PREPARATORIOS",
          str_detect(tproc, "^SUCESORIO") ~ "SUCESORIOS", #
          str_detect(tproc, "^BENEFICIO") ~ "BENEF.LITIG.SG.", #
          str_detect(tproc, "^HOMOLOGACION") ~ "HOMOLOGACIONES", 
          str_detect(tproc, "^VIOLENCIA") ~ "VIOLENCIAS", 
          # Anomalías: desclasificados, no procesos y errores
          str_detect(tproc, "OFICIO|EXHORTO|SUMARIO|COMPENSACION|MATRICULA DE COMERCIANTE|DESARCHIVO|^INFORME|^DENUNCIA|^SUPERVIVENCIA|^AUTORIZACION") ~ "noprocesos",
          str_detect(tproc, "CIVIL Y COMERCIAL|PROCESO LABORAL|PROCESO DE FAMILIA|CONCURSOS Y QUIEBRAS|^SENTENCIA|SOLICITA|PAZ") ~ "desclasificados",
          str_detect(tproc, "CONCURSO CERRADO|EXPEDIENTE INTERNO|ADMINISTRATIVO|PERSONAL|^LICENCIAS|^ARANCELES") ~ "error",
          # Otros Fueros Laboral y Familia
          str_detect(tproc, "LABORAL|TRABAJO|COMISION MEDICA|SUSTITUCION DE DEPOSITO|ENFERMEDAD PROFESIONAL|SALARIO|SINDICAL|CONCILIACION") ~ "desclasificados", 
          str_detect(tproc, "DIVORCIO|ALIMENTO|RESTRICCIONES CAPAC|REGIMEN COMUNIC|^IMPEDIMENTO DE CON") ~ "desclasificados",
          str_detect(tproc, "AMENAZA|^LESIONES|^MEDIDA DE PROTEC|GUARDA|INSCRIPCION|INTERNACION|RECONSTRUCCION DE EX|CAPACIDAD|TUTELA|SALUD MENTAL|^SU ") ~ "desclasificados",
          str_detect(tproc, "^ADOPCION|^IMPUGNACION DE RECON|^USO DE DOCUMENTO|^TESTIMONIO") ~ "desclasificados",
          str_detect(tproc, "DILIGENCIAMIENTO|^CERTIFICACIONES|^MEDIDAS|(LOPJ)|^DILIGENCIAS") ~ "desclasificados",
          TRUE ~ "OTROS"
        )) %>% 
        filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error"))
      df
  } else if(materia == "paztram") {
    df <- df %>% 
      mutate(tproc = toupper(tproc)) %>%
      mutate(gtproc_forma = case_when(
        str_detect(tproc, "^CERTIFICACION") ~ "CERTIFICACIONES",  #
        str_detect(tproc, "^AUTORIZACION") ~ "AUTORIZACIONES", #
        str_detect(tproc, "^CARTA PODER") ~ "CARTAS PODER",  #
        str_detect(tproc, "^CARTA DE POBREZA") ~ "CARTAS DE POBREZA",  #
        str_detect(tproc, "^DECLARACION JURADA") ~ "DECLARACION JURADA",  #
        str_detect(tproc, "^FORMULARIO") ~ "FORMULARIOS (ANSES,DDJJ,LEY3011,etc)",  #
        str_detect(tproc, "INFORMACION SUMARIA") ~ "INFORMACION SUMARIA",
        str_detect(tproc, "MEDIDAS ALTERNATIVAS") ~ "CONTROL MED.ALTERNAT.",
        str_detect(tproc, "RUBRICA DE LIBROS") ~ "RUBRICA DE LIBROS",
        TRUE ~ "OTROS"
      )) %>% 
      filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error"))
    df
  } else if(materia == "ecq") {
    df <- df %>% 
      mutate(tproc = toupper(tproc)) %>%
      mutate(gtproc_forma = case_when(
        str_detect(tproc, "^ORDINARIO|^ORDINARIO |USUCAP") ~ "ORDINARIOS", #
        str_detect(tproc, "^SUMARISIMO|^SUMARISIMO ") ~ "SUMARISIMO",  #
        str_detect(tproc, "^INCIDENTE|^INCIDENTE ") ~ "INCIDENTES",
        str_detect(tproc, "^MEDIDA CAUTELAR|^MEDIDA CAUTELAR ") ~ "CAUTELARES",
        str_detect(tproc, "MONITORIO") ~ "MONITORIOS",
        str_detect(tproc, "^EJECU|^PROCESO DE EJECU|^APREMIO") ~ "EJECUCIONES", #
        str_detect(tproc, "^INTERDICT") ~ "INTERDICTOS",
        str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
        str_detect(tproc, "^PREPARACION ") ~ "PREPARATORIOS",
        str_detect(tproc, "^SUCESORIO") ~ "SUCESORIOS", #
        str_detect(tproc, "^BENEFICIO") ~ "BENEF.LITIG.SG.", #
        str_detect(tproc, "^HOMOLOGACION") ~ "HOMOLOGACIONES", 
        str_detect(tproc, "^SECUESTRO PREND") ~ "SECUESTRO PRENDARIO",
        str_detect(tproc, "^CONCURSO|^QUIEBRA|^PEDIDO DE QUIEBRA") ~ "CONCURSOS/QUIEBRAS", 
        # Anomalías: desclasificados, no procesos y errores
        str_detect(tproc, "OFICIO|EXHORTO|SUMARIO|COMPENSACION|MATRICULA DE COMERCIANTE|DESARCHIVO|^INFORME|^DENUNCIA") ~ "noprocesos",
        str_detect(tproc, "CIVIL Y COMERCIAL|PROCESO LABORAL|PROCESO DE FAMILIA|^SENTENCIA|SOLICITA") ~ "desclasificados",
        str_detect(tproc, "CONCURSO CERRADO|EXPEDIENTE INTERNO|ADMINISTRATIVO|PERSONAL") ~ "error",
        # Otros Fueros Laboral y Familia
        str_detect(tproc, "LABORAL|TRABAJO|COMISION MEDICA|SUSTITUCION DE DEPOSITO|ENFERMEDAD PROFESIONAL|SALARIO|SINDICAL|CONCILIACION") ~ "desclasificados", 
        str_detect(tproc, "VIOLENCIA|DIVORCIO|ALIMENTO|RESTRICCIONES CAPAC|REGIMEN COMUNIC|^IMPEDIMENTO DE CON") ~ "desclasificados",
        str_detect(tproc, "AMENAZA|^MEDIDA DE PROTEC|GUARDA|INSCRIPCION|INTERNACION|RECONSTRUCCION DE EX|CAPACIDAD|TUTELA|SALUD MENTAL|^SU ") ~ "desclasificados",
        str_detect(tproc, "^ADOPCION|^IMPUGNACION DE RECON|^USO DE DOCUMENTO|^TESTIMONIO") ~ "desclasificados",
        TRUE ~ "OTROS"
      )) %>% 
      filter(!gtproc_forma %in% c("noprocesos", "desclasificados", "error"))
    df
  } 
  #  else if(materia == "penal") {
  #   df <- df %>% 
  #     mutate(tproc = toupper(tproc)) %>%
  #     mutate(tproc = str_trim(tproc)) %>% 
  #     mutate(tproc = str_replace_all(tproc, "SU DENUNCIA ", "")) %>% 
  #     mutate(tproc = str_replace_all(tproc, "DENUNCIA ", "")) %>% 
  #     mutate(gtproc_forma = case_when(
  #       # excluye: oficios y exhortos
  #       # clasificador de tipos según minima cantidad de palabras conincidentes con tabla de procesos (computados desde la primera)
  #       # establecer tipos de proceso, caratular con el mayor pena y agregar demás delitos en otr
  #       # provisoriamente: recaratular todo lo que sea denuncia por la tipo de delito del legajo
  #       str_detect(tproc, "DENUNCIA") ~ "DENUNCIA", 
  #       str_detect(tproc, "^ABUSO DE ARMAS") ~ "ABUSO DE ARMAS", 
  #       str_detect(tproc, "^ABUSO SEXUAL") ~ "ABUSO SEXUAL",
  #       str_detect(tproc, "AMPARO|HABEAS|INCONSTITUCIONALIDAD|ACCION DE PROHIBICION|ACCION DE EJECUCION") ~ "PROC.COSTIT.",
  #       str_detect(tproc, "^AGRESION CON ARMAS") ~ "AGRESION CON ARMAS",
  #       str_detect(tproc, "^AMENAZAS") ~ "AMENAZAS",
  #       str_detect(tproc, "^ATENTADO A LA AUTORIDAD") ~ "ATENTADO A LA AUTORIDAD",
  #       str_detect(tproc, "ESTUPEFACIENTES|23.737|23737") ~ "ESTUPEFACIENTES",
  #       str_detect(tproc, "^CALUMNIAS INJURIAS|INJURIAS CALUMNIAS") ~ "CALUMNIAS INJURIAS",
  #       str_detect(tproc, "^COACCIONES") ~ "COACCIONES",
  #       str_detect(tproc, "^CORRUPCION DE MENORES") ~ "CORRUPCION DE MENORES",
  #       str_detect(tproc, "^DAÑO") ~ "DAÑO",
  #       str_detect(tproc, "SUSTANCIAS MEDICINALES") ~ "SUMINISTRO INDEB.SUST.MED.",
  #       str_detect(tproc, "^DESOBEDIENCIA") ~ "DESOBEDIENCIA",
  #       str_detect(tproc, "^ENCUBRIMIENTO") ~ "ENCUBRIMIENTO",
  #       str_detect(tproc, "^ESTAFA") ~ "ESTAFA",
  #       str_detect(tproc, "^EXTORSION") ~ "EXTORSION",
  #       str_detect(tproc, "^GROOMING") ~ "GROOMING",
  #       str_detect(tproc, "^HOMICIDIO") ~ "HOMICIDIO",
  #       str_detect(tproc, "^HURTO") ~ "HURTO",
  #       str_detect(tproc, "^INCENDIO") ~ "INCENDIO",
  #       str_detect(tproc, "^LESIONES") ~ "LESIONES",
  #       str_detect(tproc, "^PECULADO") ~ "PECULADO",
  #       str_detect(tproc, "^PORTACION DE ARMA") ~ "PORTACION DE ARMA",
  #       str_detect(tproc, "^PRIVACION ILEGITIMA DE LA LIBERTAD") ~ "PRIVACION ILEGITIMA DE LA LIBERTAD",
  #       str_detect(tproc, "^RECEPTACION SOSPECHOSA DE BIENES") ~ "RECEPTACION SOSPECHOSA DE BIENES",
  #       str_detect(tproc, "^RESISTENCIA A LA AUTORIDAD|RESISTENCIA") ~ "RESISTENCIA A LA AUTORIDAD",
  #       str_detect(tproc, "^ROBO") ~ "ROBO",
  #       str_detect(tproc, "^ABIGEATO") ~ "ABIGEATO",
  #       str_detect(tproc, "^INTIMIDACION PUBLICA") ~ "INTIMIDACION PUBLICA",
  #       str_detect(tproc, "TENENCIA DE ARMA DE FUEGO") ~ "TENENCIA DE ARMA DE FUEGO",
  #       str_detect(tproc, "TENTATIVA DE HOMICIDIO") ~ "TENTATIVA DE HOMICIDIO",
  #       str_detect(tproc, "^USURPACION") ~ "USURPACION",
  #       str_detect(tproc, "^DEFRAUDACION") ~ "DEFRAUDACION",
  #       str_detect(tproc, "^EXHIBICIONES OBSCENAS") ~ "EXHIBICIONES OBSCENAS",
  #       str_detect(tproc, "^FALSIFICACION DE NUMERACION") ~ "FALSIFICACION DE NUMERACION",
  #       str_detect(tproc, "^RETENCION INDEBIDA") ~ "RETENCION INDEBIDA",
  #       str_detect(tproc, "^INSOLVENCIA FRAUDULENTA") ~ "INSOLVENCIA FRAUDULENTA",
  #       str_detect(tproc, "^VIOLACION DE DOMICILIO") ~ "VIOLACION DE DOMICILIO",
  #       str_detect(tproc, "^VIOLENCIA DE GENERO|VIOLENCIA DE GÉNERO") ~ "VIOLENCIA DE GENERO",
  #       str_detect(tproc, "^VIOLENCIA FAMILIAR|9.198") ~ "VIOLENCIA FAMILIAR",
  #       str_detect(tproc, "13.944|13944") ~ "INFRACCION A LA LEY Nº 13.944",
  #       str_detect(tproc, "14.346|14346") ~ "INFRACCION A LA LEY Nº 14.346",
  #       str_detect(tproc, "20.429|20429") ~ "INFRACCION A LA LEY Nº 20.429",
  #       str_detect(tproc, "22.362|22362") ~ "INFRACCION A LA LEY Nº 22.362",
  #       str_detect(tproc, "24.270|24270|IMPEDIMENTO DE CONTACTO") ~ "INFRACCION A LA LEY Nº 24.270",
  #       str_detect(tproc, "ACTUACIONES DE OFICIO|ACTUACIONES PARA ESTABLECER|INVESTIGACION DE OFICIO|ACTUACIONES") ~ "ACTUACIONES DE OFICIO",
  #       str_detect(tproc, "^ALLANAMIENTO") ~ "ALLANAMIENTOS",
  #       str_detect(tproc, "^SU MUERTE|SU PRESENTACION|EN GRADO DE TENTATIVA") ~ "OTROS",
  #       #####################################################################
  #       str_detect(tproc, "^ADMINISTRATIVO|^ARCHIVO|^AUTORIZACION|CONCURSO CERRADO") ~ "noproceso",
  #       str_detect(tproc, "^CONCURSOS|^DESIGNACION|^EFECTOS SECUESTRADOS|^EXHORTO|^INFORMES") ~ "noproceso",
  #       str_detect(tproc, "^LEGAJO|^OFICIO|^PERSONAL|^SENTENCIA|^SU PRESENTACION|^SU SITUACION") ~ "noproceso", 
  #       str_detect(tproc, "^EJECUCION DE SENTENCIA|^EJECUTIVO|FALLECIMIENTO|IMPEDIMENTO DE CONTACTO|^INCIDENTE|INSTRUCCION") ~ "desclasificado",
  #       str_detect(tproc, "^LIBRAMIENTO DE CHEQUE|^MEDIDA CAUTELAR|^OTRO|^SOLICITUD") ~ "desclasificado",
  #       str_detect(tproc, "^VARIOS|^TESTIMONIOS|Y OTROS") ~ "desclasificado",
  #       str_detect(tproc, "^JURISPRUDENCIA|LEY") ~ "error", 
  #       TRUE ~ "sd")) %>% 
  #     filter(!gtproc_forma %in% c("noproceso", "desclasificado", "error"))
  #   df
  # }
  df
}


listar_presentaciones <- function() {
  
  fuero <- str_c(str_sub(tolower(fuero), 1,3), collapse = "|") %>% 
    str_replace("civ", "cco") %>% 
    str_replace("pen|pej", "pen") %>% 
    str_replace("pme", "oma|equ")
  
  if (instancia1 & instancia2 & instancia3) {org <- "oma|oga|jez|jdopna|jdo|cam|sal"} 
  else if (instancia1 & instancia2) {org <- "oma|oga|jez|jdopna|jdo|cam"} 
  else if (instancia1) {org <- "oma|oga|jez|jdopna|jdo|camcad"}
  else if (instancia2) {org <- "cam"}
  else if (instancia3) {org <- "sal"}
  
  if (is.na(circ)) {
    circ <- "Gualeguaychú|Uruguay|Concordia|Paraná|Diamante|Federación|Federal|San Salvador|Tala|Victoria|Islas del Ibicuy|Chajarí|Colón|Gualeguay|La Paz|Nogoyá|Villaguay|Feliciano" 
  } else {
    circ <- circ
  }
  
  if(fuero == "ecq") {
    poblacion_informada <- poblacion_total %>% 
      filter(str_detect(materia, "eje|cqb")) %>% 
      filter(str_detect(organismo, !!org)) %>% 
      filter(str_detect(circunscripcion, !!circ))
  } else {
    poblacion_informada <- poblacion_er %>% 
      filter(str_detect(organismo, !!fuero)) %>% 
      filter(str_detect(organismo, !!org)) %>% 
      filter(str_detect(circunscripcion, !!circ))
  }
  
  
  presentaciones_justat <- DB_PROD() %>% tbl("submission") %>% 
    select(-input) %>% filter(data_interval_start >= start_date, data_interval_end <= end_date) %>%
    filter(enabled == TRUE) %>% filter(iep %in% poblacion_informada$organismo) %>%
    collect() %>% 
    mutate(mes_informado =  lubridate::month(data_interval_start)) %>% 
    mutate(año_informado = lubridate::year(data_interval_start)) %>% 
    arrange(jurisdiction, iep, año_informado, mes_informado) %>% 
    mutate(datetime = stringr::str_sub(datetime, 1,10)) %>% 
    rowwise() %>% 
    mutate(mes_informado = str_c(mes_informado, " (", id_submission, "/", datetime, ")", sep = "")) %>% 
    ungroup() %>% 
    group_by(iep, operation) %>% 
    summarise(presentaciones = n(), 
              periodos_informados = str_c(mes_informado, collapse = ", ")) %>% 
    left_join(apgyeJusEROrganization::OPERATION_DESCRIPTION, 
              by = c("operation" = "operacion")) %>% 
    left_join(poblacion_er[, c("organismo", "organismo_descripcion", 
                                  "circunscripcion")], by = c("iep" = "organismo")) %>% 
    ungroup() %>% 
    select(circunscripcion, organismo_descripcion, operacion_descripcion, 
           'mes (nro_file-fechahora)' = periodos_informados, presentaciones, -iep) %>% 
    group_by(circunscripcion, organismo_descripcion) %>% 
    arrange(circunscripcion, organismo_descripcion, operacion_descripcion) %>% 
    do(janitor::adorn_totals(.)) %>% 
    ungroup() %>% 
    mutate(organismo_descripcion = abbreviate(organismo_descripcion, minlength = 20)) %>% 
    rename(organismo = organismo_descripcion) %>% 
    mutate(circunscripcion = abbreviate(circunscripcion, minlength = 8)) %>% 
    mutate(operacion_descripcion = abbreviate(operacion_descripcion, minlength = 20))
  
  presentaciones_justat
}

primarias <- function (organismo, operacion, start_date, end_date) {
  resultado <- DB_PROD() %>% 
    apgyeTableData(!!operacion) %>% 
    apgyeDSL::interval(start_date, end_date) %>% 
    filter(iep == organismo) %>% 
    collect()
}

reimputaciones <- function (df) {
  # error en el informe de iniciados jdopaz mancilla 2018
  # correo Juzgado de Paz - Gdor. Mansilla E.R. <jdopazcat3-man@jusentrerios.gov.ar> 19/03/19
  # respuesta coreo apge 19/03/19
  if(any(df$iep %in% c("jdopaz0000man")) & 
     any(df$data_interval_start > "2018-01-01") & 
     any(df$data_interval_end >= "2019-01-01") & 
     any(colnames(df) %in% "iniciados")) {
    df <- df %>% 
      filter(!(iep == "jdopaz0000man" & str_detect(tipo_procesos, "RESIDUAL")))
    df
  }
  
  df
}

get_agentes_inactivos <- function(agentes){
  
  # top_3_planta_ocupada <- DB_SINGLE_CONNECTION %>% apgyeDSL::apgyeTableData('personal_planta_ocupada') %>% select(id_submission, data_interval_start) %>% distinct() %>% top_n(3, data_interval_start) %>% collect() %>% .$id_submission
  # top_3_personas <- DB_SINGLE_CONNECTION %>% apgyeDSL::apgyeTableData('personal_personas') %>% select(id_submission, data_interval_start) %>% distinct() %>% top_n(3, data_interval_start) %>% collect() %>% .$id_submission

  personal_data <- DB_PROD() %>%
    tbl("gh_personas") %>%
    filter(idagente %in% agentes) %>%
    collect()

  # library(dplyr)
  # prueba_Personal_Actual <-  DB_PROD() %>%
  #   tbl("gh_personal_actual") %>%
  #   collect()
  # 
  # prueba_Personal_Historico <-  DB_PROD() %>%
  #   tbl("gh_personal_historico") %>%
  #   collect()
  # 
  # prueba_Personal_Distinct <-  DB_PROD() %>%
  #   tbl("gh_personal_distinct") %>%
  #   collect()
  # 
  # View(prueba_Personal_Distinct)
  # prueba_Personal_Distinct$idagente
  # names(prueba_Personal_Actual) == names(prueba_Personal_Historico)
  
}


existepresentacion <- function(poblacion, start_date, end_date, operacion) {
  
  operacion = rlang::enexpr(operacion)
 
  presentaciones_justat <- DB_PROD() %>% tbl("submission") %>% 
    select(-input) %>% filter(data_interval_start >= start_date, data_interval_end <= end_date) %>%
    filter(enabled == TRUE) %>% filter(iep %in% poblacion$organismo) %>%
    collect() 
  
  if (!is.null(presentaciones_justat$operation)) {
    
    resultado = any(str_detect(presentaciones_justat$operation, operacion))
      
  } else{
    
    resultado = FALSE
    
  }
    
  resultado
}


inic_xcetal1sem18 <- function(poblacion, operacion) {
  inic <- DB_PROD() %>% 
    apgyeDSL::apgyeTableData("CETAL_XL") %>% 
    filter(iep %in% poblacion$organismo) %>% 
    filter(!is.na(finicio), !is.na(fmov)) %>% 
    filter(!grepl("OFICIO|EXHORTO", tproc)) %>% 
    mutate(finicio = dmy(finicio)) %>%
    mutate(fmov = dmy(fmov)) %>%
    filter(finicio >= start_date, finicio < "2018-07-01" ) %>% 
    group_by(nro, caratula) %>%                   # se quitan causas que se han movido entre organismos
    filter(fmov == max(fmov, na.rm = TRUE)) %>%   # esto se hace con el fin de remover duplicados
    ungroup()  %>%                                # para los casos en que se hacen pases por competencia
    select(iep, nro, caratula, tproc, finicio)  %>% 
    dplyr::union(
      DB_PROD() %>% 
        apgyeDSL::apgyeTableData(!!operacion) %>% 
        filter(iep %in% poblacion$organismo) %>% 
        filter(!grepl("OFICIO|EXHORTO", tproc)) %>% 
        mutate(finicio = dmy(finicio)) %>%
        filter(!is.na(finicio)) %>% 
        filter(finicio >= start_date, finicio < end_date ) %>% 
        select(iep, nro, caratula, tproc, finicio)  
    ) %>% 
    collect() 
  inic
}
