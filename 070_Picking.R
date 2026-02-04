#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Archivos de Facturas
tFilesFactura <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Pedidos_Pendientes", "01_Tablas", "Facturas"), pattern = "Facturas_\\d{4}\\.csv$", full.names = TRUE)

#Archivos Shipment Luxottica
tLuxotticaShipmentFiles <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Luxottica", "01_Tablas", "Allocated & Shipments"), pattern = "^GVI_MX40_.*\\.xlsx?$", full.names = TRUE)

#Archivos Allocated Luxottica
tLuxotticaAllocatedFiles <- list.files(path = file.path(rUser, rSharePoint, "Reportes" , "Luxottica", "01_Tablas", "Allocated & Shipments"), pattern = ".*Allocated.*\\.xlsx?$", full.names = TRUE)

#================ Ejecucion ===================
#Filtramos Archivos de Facturas de los ultimos 12 meses

#Define mes limite
cMesLimite <- cPrimerDiaMes - months(11)

#Filtra nombre de archivos de Facturas recientes
q000NombreFacturasFiles <- tFilesFactura %>% 
  keep(~{
    # Extraer año y mes del nombre
    numeros <- str_extract(., "Facturas_(\\d{4})") %>% str_remove("Facturas_")
    if (is.na(numeros)) return(FALSE)
    
    año <- 2000 + as.numeric(substr(numeros, 1, 2))
    mes <- as.numeric(substr(numeros, 3, 4))
    fecha_archivo <- make_date(año, mes, 1)
    
    fecha_archivo >= cMesLimite & fecha_archivo <= cPrimerDiaMes
  })

#Leyendo archvos de Facturas
tFacturas <- map_dfr(q000NombreFacturasFiles, ~{read.csv(.x) %>%
                         mutate(archivo_origen = basename(.x))}) %>% 
  rename_all(toupper)

#Leyendo archivos de Shipment de Luxottica
tShipments <- map_dfr(tLuxotticaShipmentFiles, fLeerShipmentsArchivoExcel) %>% 
  rename_all(toupper)

#Leyendo archivos de Allocated de Luxottica
tAllocated <- map_dfr(tLuxotticaAllocatedFiles, fLeerAllocatedArchivoExcel) %>% 
  rename_all(toupper)

#Cruzando informacion
q001fact <- tFacturas %>% 
  mutate(ID_PS = paste(FOLIO_PEDIDO, SKU, sep = "|")) %>% 
  select(ID_PS, FEC_REC)

q001PPFactura <- tPedidos_Pendientes %>% 
  select(ID_PEDIDO, STARS, SKU, EAN, FECHA_CAP, FECHA_PROMESA) %>% 
  na.omit(STARS) %>% 
  mutate(STARS = str_pad(STARS, 10, "left", pad = "0")) %>% 
  mutate(ID_SE = paste(STARS, EAN, sep = "|")) %>% 
  mutate(ID_PS = paste(ID_PEDIDO, SKU, sep = "|")) %>% 
  left_join(q001_fact, by = "ID_PS") %>% 
  select(-STARS, -EAN, -SKU, -ID_PS) %>% 
  mutate(
    FECHA_CAP = as.Date(FECHA_CAP, format = "%d/%m/%Y"),
    FECHA_PROMESA = as.Date(FECHA_PROMESA, format = "%d/%m/%Y"),
    FEC_REC = as.Date(FEC_REC, format = "%d/%m/%Y"))

#
q001ShipFormat <- tShipments %>% 
  rename(
    SHIPMENT = "SAP TRANSPORTS - TRANSPORT NUMBER",
    BANNER = "BANNER",
    SHIPMENT_QTY = "DELIVERIES - TOTAL GOODS ISSUE QTY",
    SALES_DOC = "DELIVERIES DETAIL - ORDER DOCUMENT NUMBER", 
    UPC_CODE = "ITEM - UPC CODE",
    GOODS_ISSUE_DATE = "GOODS ISSUE DATE: DATE",
    ARCHIVO_SHIP = "ARCHIVO_ORIGEN"
  ) %>% 
  mutate(ID_SD =  paste(SALES_DOC , UPC_CODE, sep = "|")) %>% 
  mutate(SHIPMENT = str_pad(SHIPMENT, 10, "left", pad = "0")) %>% 
  mutate(ID_SE = paste(SHIPMENT, UPC_CODE, sep = "|")) %>% 
  left_join(q001PPFactura, by = "ID_SE") %>% 
  select(- ID_SE)

#
q001AllocFormat <- tAllocated %>% 
  rename(   ###Corrigiendo nombres para quye sean más legibles
    BANNER = "BANNER",
    SALES_DOC = "SALES DOC.",
    CREATED_DATE = "CREATED ON",
    UPC_CODE = "EAN/UPC",
    ORDERED_QTY = "ORDERED QTY",
    ALLOCATED_QTY = "ALLOCATED QTY",
    # archivo_origen ya está bien
    ARCHIVO_ALLOCATED = "ARCHIVO_ORIGEN"
  ) %>% 
  mutate(ID_SD =  paste(SALES_DOC , UPC_CODE, sep = "|")) 

#
q001EnviosGroup <- q001ShipFormat %>% 
  group_by(ID_SD) %>%
  summarise(
    T_ENVIADO = sum(SHIPMENT_QTY, na.rm = TRUE),
    P_ENVIO = min(GOODS_ISSUE_DATE, na.rm = TRUE),
    U_ENVIO = max(GOODS_ISSUE_DATE, na.rm = TRUE),
    N_ENVIOS = n(),
    
    # Nuevas métricas con los campos adicionales
    FECHA_CAP = first(FECHA_CAP),  # Asumiendo que es la misma para todos los envíos del mismo ID_SD
    FECHA_PROMESA = first(FECHA_PROMESA),
    FECHA_RECEPCION = first(FEC_REC),
    
    # Tiempos específicos por shipment
    LEAD_TIMER = as.numeric(difftime(FECHA_RECEPCION, P_ENVIO, units = "days")),
    
    .groups = 'drop'
  )

#Analisis de tiempo
#Unir con las solicitudes
q002AnlssTime <- q001AllocFormat %>% 
  left_join(q001EnviosGroup, by = "ID_SD") %>%
  mutate(
    # ===== TIEMPOS DE SURTIDO =====
    # Tiempo desde solicitud hasta primer envío
    T_1E_ENVIO = as.numeric(difftime(P_ENVIO, CREATED_DATE, units = "days")),
    
    # Tiempo desde solicitud hasta último envío (si hay múltiples envíos)
    T_ULT_ENVIO = as.numeric(difftime(U_ENVIO, CREATED_DATE, units = "days")),
    
    # Tiempo desde primer envío hasta recepción
    T_ENVIO_REC = as.numeric(difftime(FECHA_RECEPCION, P_ENVIO, units = "days")),
    
    # ===== CUMPLIMIENTO DE FECHAS PROMETIDAS =====
    # ¿Llegó antes de la fecha prometida?
    CUMPLIO_PROMESA = FECHA_RECEPCION <= FECHA_PROMESA,
    DIAS_VAR_PROMESA = as.numeric(difftime(FECHA_RECEPCION, FECHA_PROMESA, units = "days")),
    
    # ===== EFICIENCIA DE SURTIDO =====
    # Estado del surtido
    BACK_ORDER = ORDERED_QTY - T_ENVIADO,
    PORCENTAJE_SURTIDO = ifelse(ORDERED_QTY > 0, T_ENVIADO / ORDERED_QTY * 100, 0),
    ESTADO_SURTIDO = case_when(
      PORCENTAJE_SURTIDO >= 100 ~ "COMPLETAMENTE SURTIDO",
      PORCENTAJE_SURTIDO > 0 ~ "PARCIALMENTE SURTIDO", 
      is.na(T_ENVIADO) ~ "SIN SURTIR",
      TRUE ~ "SIN SURTIR"
    ),
    
    # Eficiencia de allocated vs enviado
    DIFERENCIA_ALLOCATED_ENVIADO = ALLOCATED_QTY - T_ENVIADO,
    eficiencia_allocated = ifelse(ALLOCATED_QTY > 0, T_ENVIADO / ALLOCATED_QTY * 100, 0)
  ) %>% 
  mutate(BACK_ORDER = case_when(
    is.na(BACK_ORDER) ~ ALLOCATED_QTY,
    TRUE ~ BACK_ORDER)) %>% 
  select(-ARCHIVO_ALLOCATED, -ID_SD)

#Lista de data frames a conservar
vGuarda <- c("tPicking") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)