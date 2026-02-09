#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Archivos de Facturas

#Filtramos Archivos de Facturas de los ultimos 12 meses

#Define mes limite
cMesLimite <- cPrimerDiaMes - months(12)

#Listamos archivos de facturas y filtramos por fecha
tFilesFactura <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Pedidos_Pendientes", "01_Tablas", "Facturas"), pattern = "Facturas_\\d{4}\\.csv$", full.names = TRUE) %>% 
  keep(~{
    
    #Extraer año y mes del nombre
    q000FechaArchivo <- str_extract(., "Facturas_(\\d{4})") %>% #obtiene el año y mes del nombre del archivo
      str_remove("Facturas_") #Elimina el prefijo para quedarse solo con el año y mes
    
    if (is.na(q000FechaArchivo)) return(FALSE) #Si no hay archivos termina funcion
    
    q000Anio <- 2000 + as.numeric(substr(q000FechaArchivo, 1, 2)) #Define el Anio del nombre de los archivos
    q000Mes <- as.numeric(substr(q000FechaArchivo, 3, 4)) #Define el Mes del nombre de los archivos
    q000FechaArchivo <- make_date(q000Anio, q000Mes, 1) #Convierte el año y mes a una fecha
    
    q000FechaArchivo >= cMesLimite & q000FechaArchivo <= cPrimerDiaMes #Filtra archivos que estén dentro del rango de fecha 
  })

#Leyendo archivos de Facturas
tFacturas <- map_dfr(tFilesFactura, ~{ 
  read.csv(.x) %>%
    mutate(archivo_origen = basename(.x))}) %>% 
  rename_all(toupper) %>% 
  mutate(FEC_REC = as.Date(FEC_REC, format = "%Y-%m-%d")) %>%
  mutate(ID_PS = paste(FOLIO_PEDIDO, SKU, sep = "|"))

#Archivos Shipment Luxottica
tLuxotticaShipmentFiles <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Luxottica", "01_Tablas", "Allocated & Shipments"), pattern = "^GVI_MX40_.*\\.xlsx?$", full.names = TRUE)

#Leyendo archivos de Shipment de Luxottica
tShipments <- map_dfr(tLuxotticaShipmentFiles, fLeerShipmentsArchivoExcel) %>% 
  rename_all(toupper)

#Archivos Allocated Luxottica
tLuxotticaAllocatedFiles <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Luxottica", "01_Tablas", "Allocated & Shipments"), pattern = ".*Allocated.*\\.xlsx?$", full.names = TRUE)

#Leyendo archivos de Allocated de Luxottica
tAllocated <- map_dfr(tLuxotticaAllocatedFiles, fLeerAllocatedArchivoExcel) %>% 
  rename_all(toupper)

#================ Ejecucion ===================

#Pedidos pendientes con fecha de recibo en facturas
q001PPFactura <- tPedidos_Pendientes %>% 
  select(ID_PEDIDO, STARS, SKU, EAN, FECHA_CAP, FECHA_PROMESA) %>% 
  na.omit(STARS) %>% #Filtra los pedidos de Luxottica
  mutate(STARS = fRight(STARS, 8)) %>% #Formato
  mutate(ID_SE = paste(STARS, EAN, sep = "|")) %>% 
  mutate(ID_PS = paste(ID_PEDIDO, SKU, sep = "|")) %>% 
  left_join(tFacturas[,c("ID_PS", "FEC_REC")], by = "ID_PS") #Obtenemos la Fecha de Recibo de la data de facturas
  
#A los Shipments les cruzamos la info de facturas y pedidos pendientes
q001ShipFormat <- tShipments %>% 
  rename(
    SHIPMENT = "SAP TRANSPORTS - TRANSPORT NUMBER",
    BANNER = "BANNER",
    SHIPMENT_QTY = "DELIVERIES - TOTAL GOODS ISSUE QTY",
    SALES_DOC = "DELIVERIES DETAIL - ORDER DOCUMENT NUMBER", 
    UPC_CODE = "ITEM - UPC CODE",
    GOODS_ISSUE_DATE = "GOODS ISSUE DATE: DATE",
    ARCHIVO_SHIP = "ARCHIVO_ORIGEN"
  ) %>% #Renombra Columnas
  mutate(UPC_CODE = fRight(UPC_CODE, 13)) %>% #Formato EAN
  mutate(SHIPMENT = fRight(SHIPMENT, 8)) %>% #Formato STARS
  mutate(ID_SE = paste(SHIPMENT, UPC_CODE, sep = "|")) %>% 
  mutate(ID_SD =  paste(SALES_DOC , UPC_CODE, sep = "|")) %>%
  left_join(q001PPFactura[,c("ID_SE", "ID_PEDIDO", "FECHA_CAP", "FECHA_PROMESA", "FEC_REC")], by = "ID_SE") #Cruza informacion de pedidos pendientes y facturas para obtener la fecha de captura, fecha promesa y fecha de recibo

#Agrupamos por Sales Doc / Ean para obtener el total enviado, fecha del primer envío, fecha del último envío, número de envíos, fecha de captura, fecha promesa, fecha de recepción y lead time desde el primer envío hasta la recepción.
q001EnviosGroup <- q001ShipFormat %>% 
  group_by(SALES_DOC, UPC_CODE) %>%
  summarise(
    T_ENVIADO = sum(SHIPMENT_QTY, na.rm = TRUE), #Total de Ean por Sales Doc
    P_ENVIO = min(GOODS_ISSUE_DATE, na.rm = TRUE), #Fecha del primer envío
    U_ENVIO = max(GOODS_ISSUE_DATE, na.rm = TRUE), #Fecha del último envío
    N_ENVIOS = n(), #Numero de Envios
    FECHA_CAP = first(FECHA_CAP),  #Asumiendo que es la misma para todos los envíos del mismo ID_SD
    FECHA_PROMESA = first(FECHA_PROMESA), #Fecha promesa del sales Doc
    FECHA_RECEPCION = first(FEC_REC), #Fecha de recepción del Sales Doc
    LEAD_TIMER = as.numeric(difftime(FECHA_RECEPCION, P_ENVIO, units = "days")), #Lead time desde el primer envío hasta la recepción
    .groups = 'drop'
  ) %>% 
  mutate(ID_SD = paste(SALES_DOC, UPC_CODE, sep = "|")) 

#Formato a info de allocated
q001AllocFormat <- tAllocated %>% 
  rename(   
    BANNER = "BANNER",
    SALES_DOC = "SALES DOC.",
    CREATED_DATE = "CREATED ON",
    UPC_CODE = "EAN/UPC",
    ORDERED_QTY = "ORDERED QTY",
    ALLOCATED_QTY = "ALLOCATED QTY",
    ARCHIVO_ALLOCATED = "ARCHIVO_ORIGEN"
  ) %>% 
  mutate(UPC_CODE = fRight(UPC_CODE, 13)) %>% 
  mutate(ID_SD =  paste(SALES_DOC , UPC_CODE, sep = "|")) 

#Analisis de tiempo
q002AnlssTime <- q001AllocFormat %>% 
  left_join(q001EnviosGroup[,c("ID_SD", "T_ENVIADO", "P_ENVIO", "U_ENVIO", "FECHA_RECEPCION", "FECHA_PROMESA")], by = "ID_SD") %>%
  mutate(T_1E_ENVIO = as.numeric(difftime(P_ENVIO, CREATED_DATE, units = "days"))) %>% #Tiempo desde solicitud hasta primer envío
  mutate(T_ULT_ENVIO = as.numeric(difftime(U_ENVIO, CREATED_DATE, units = "days"))) %>% #Tiempo desde solicitud hasta último envío (si hay múltiples envíos)
  mutate(T_ENVIO_REC = as.numeric(difftime(FECHA_RECEPCION, P_ENVIO, units = "days"))) %>% #Tiempo desde primer envío hasta recepción
  mutate(CUMPLIO_PROMESA = FECHA_RECEPCION <= FECHA_PROMESA) %>% #¿Llegó antes de la fecha prometida?
  mutate(DIAS_VAR_PROMESA = as.numeric(difftime(FECHA_RECEPCION, FECHA_PROMESA, units = "days"))) %>% 
  mutate(BACK_ORDER = ORDERED_QTY - T_ENVIADO) %>%  #Estado del surtido
  mutate(PORCENTAJE_SURTIDO = ifelse(ORDERED_QTY > 0, T_ENVIADO / ORDERED_QTY * 100, 0)) %>% 
  mutate(ESTADO_SURTIDO = case_when(
      PORCENTAJE_SURTIDO >= 100 ~ "COMPLETAMENTE SURTIDO",
      PORCENTAJE_SURTIDO > 0 ~ "PARCIALMENTE SURTIDO", 
      is.na(T_ENVIADO) ~ "SIN SURTIR",
      TRUE ~ "SIN SURTIR"
    )) %>% 
  mutate(DIFERENCIA_ALLOCATED_ENVIADO = ALLOCATED_QTY - T_ENVIADO) %>%  #Eficiencia de allocated vs enviado
  mutate(EFICIENCIA_ALLOCATED = ifelse(ALLOCATED_QTY > 0, T_ENVIADO / ALLOCATED_QTY * 100, 0)) %>% 
  mutate(BACK_ORDER = ifelse(is.na(BACK_ORDER), ALLOCATED_QTY, BACK_ORDER)) %>%
  arrange(desc(CREATED_DATE)) %>% 
  select(BANNER, SALES_DOC, CREATED_DATE, UPC_CODE, ORDERED_QTY, ALLOCATED_QTY, T_ENVIADO, P_ENVIO, U_ENVIO, FECHA_RECEPCION, FECHA_PROMESA, T_1E_ENVIO, T_ULT_ENVIO, T_ENVIO_REC, CUMPLIO_PROMESA, DIAS_VAR_PROMESA, BACK_ORDER, PORCENTAJE_SURTIDO, ESTADO_SURTIDO, DIFERENCIA_ALLOCATED_ENVIADO, EFICIENCIA_ALLOCATED)

#Escribe Reporte
write.csv(q002AnlssTime, file.path(rReportes, paste("PICKING.csv", sep = "")), row.names = FALSE)

#Data frame final a guardar en el environment
tPicking <- q002AnlssTime %>% 
  left_join(tArt_Cat[,c("EAN", "TIPO", "ID_LINEA", "PACK", "SKU")], by = c("UPC_CODE" = "EAN")) %>% 
  mutate(ID_EMPRESA = case_when(
    BANNER == "GV MAS VISION MX" ~ "11",
    BANNER == "GV OPTICAS LUX MX" ~ "2",
    #BANNER == "GV OPTICAS LUX MX" ~ "7",
    TRUE ~ NA_character_  # Corrección: NA explícito para tipo numerico
  )) %>% 
  mutate(PACK = ifelse(ID_EMPRESA != c(11) | is.na(PACK) | nchar(PACK) == 0, "-", PACK)) %>% #Solo se considera Pack para Mas Vision
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% #Agrupacion
  summarise(PICKING = sum(BACK_ORDER)) %>% 
  filter(PICKING > 0) %>%
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Lista de data frames a conservar
vGuarda <- c("tPicking") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)