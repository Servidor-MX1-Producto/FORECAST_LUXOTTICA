#================ About ================
# Pasos de ejecución:
#   1.- Extraer información de Qlik
#   2.- Calcular Cantidad y Volumen
#   3.- Exportar resultados

{
  if (!require("readxl")) install.packages("readxl")
  library("readxl")
  if (!require("dplyr")) install.packages("dplyr")
  library("dplyr")
  if (!require("lubridate")) install.packages("lubridate")
  library("lubridate")
  if (!require("stringi")) install.packages("stringi")
  library("stringi")
  if (!require("stringr")) install.packages("stringr")
  library("stringr")
  if (!require("tidyr")) install.packages("tidyr")
  library("tidyr")
  if (!require("rio")) install.packages("rio")
  library("rio")
  if (!require("writexl")) install.packages("writexl")
  library("writexl")
  if (!require("ISOweek")) install.packages("ISOweek")
  library("ISOweek")
  if (!require("purrr")) install.packages("purrr")
  library("purrr")
  
  #Zona Horaria
  Sys.setenv(TZ = "Etc/GMT+6")
}

#================ Funciones ================
#Funcion para obtener el path de usuario
fGetUserPath <- function(){
      
  vUserPath <- getwd()  
  vUserPathExpr <- gregexpr("/", vUserPath, fixed = TRUE)
  vUserPathExpr <- unlist(vUserPathExpr)
  vUser <- substr(vUserPath, 1, vUserPathExpr[3] - 1)
    
}
  
#Funcion derecha
fRight <- function(x, n){
  
  cText <- paste("0000000000000", x, sep = "") 
  substr(cText, nchar(cText) - (n + 1), nchar(cText))
  
}

#Funcion para leer archivos de Shipment
fLeerShipmentsArchivoExcel <- function(cArchivo) {
  cat("Leyendo:", basename(cArchivo), "\n")
  
  tryCatch({
    # Primero leer todas las columnas como texto para evitar problemas
    datos <- read_excel(cArchivo, col_types = "text")
    
    # Verificar que las columnas requeridas existen
    columnas_requeridas <- c(
      "SAP Transports - Transport Number",
      "Banner", 
      "Deliveries - TOTAL Goods Issue Qty",
      "Deliveries Detail - Order Document Number",
      "Item - UPC Code",
      "Goods Issue Date: Date"
    )
    
    # Verificar columnas faltantes
    columnas_faltantes <- setdiff(columnas_requeridas, names(datos))
    if (length(columnas_faltantes) > 0) {
      warning("Columnas faltantes en ", basename(cArchivo), ": ", 
              paste(columnas_faltantes, collapse = ", "))
    }
    
    # Seleccionar solo las columnas que existen y son requeridas
    columnas_a_mantener <- intersect(columnas_requeridas, names(datos))
    datos <- datos %>% select(all_of(columnas_a_mantener))
    
    # Convertir a los tipos de datos correctos
    if ("Deliveries - TOTAL Goods Issue Qty" %in% names(datos)) {
      datos$`Deliveries - TOTAL Goods Issue Qty` <- as.numeric(datos$`Deliveries - TOTAL Goods Issue Qty`)
    }
    
    if ("Deliveries Detail - Order Document Number" %in% names(datos)) {
      datos$`Deliveries Detail - Order Document Number` <- as.numeric(datos$`Deliveries Detail - Order Document Number`)
    }
    
    if ("Item - UPC Code" %in% names(datos)) {
      datos$`Item - UPC Code` <- as.numeric(datos$`Item - UPC Code`)
    }
    
    if ("Goods Issue Date: Date" %in% names(datos)) {
      datos$`Goods Issue Date: Date` <- as.Date(as.numeric(datos$`Goods Issue Date: Date`), origin = "1899-12-30")
    }
    
    # Agregar columna de origen
    datos$archivo_origen <- basename(cArchivo)
    
    return(datos)
    
  }, error = function(e) {
    warning("Error al leer el cArchivo ", cArchivo, ": ", e$message)
    return(NULL)
  })
}

#Funcion para leer archivos de Allocated
fLeerAllocatedArchivoExcel <- function(cArchivo) {
  cat("Leyendo cArchivo allocated:", basename(cArchivo), "\n")
  
  tryCatch({
    # Función para detectar si una fila contiene los encabezados esperados
    detectar_fila_encabezados <- function(cArchivo) {
    # Leer primera fila
    fila1 <- read_excel(cArchivo, n_max = 1, col_names = FALSE)
    # Leer segunda fila
    fila2 <- read_excel(cArchivo, n_max = 1, col_names = FALSE, skip = 1)
    
    encabezados_esperados <- c("Banner", "Sales", "Created", "EAN", "Ordered", "Allocated")
    
    # Verificar cuántos encabezados esperados están en cada fila
    coincidencias_fila1 <- sum(sapply(encabezados_esperados, function(x) any(grepl(x, as.character(fila1), ignore.case = TRUE))))
    coincidencias_fila2 <- sum(sapply(encabezados_esperados, function(x) any(grepl(x, as.character(fila2), ignore.case = TRUE))))
      
      # Elegir la fila con más coincidencias
      if (coincidencias_fila2 > coincidencias_fila1) {
        return(1)  # saltar 1 fila
      } else {
        return(0)  # no saltar filas
      }
    }
    
    # Detectar si necesitamos saltar una fila
    skip_filas <- detectar_fila_encabezados(cArchivo)
    
    # Leer el archivo (saltando filas si es necesario)
    datos <- read_excel(cArchivo, col_types = "text", skip = skip_filas)
    
    columnas_requeridas <- c(
      "Banner",
      "Sales Doc.", 
      "Created On",
      "EAN/UPC",
      "Ordered QTY",
      "Allocated Qty"
    )
    
    columnas_faltantes <- setdiff(columnas_requeridas, names(datos))
    if (length(columnas_faltantes) > 0) {
      warning("Columnas faltantes en ", basename(cArchivo), ": ", 
              paste(columnas_faltantes, collapse = ", "))
    }
    
    columnas_a_mantener <- intersect(columnas_requeridas, names(datos))
    datos <- datos %>% select(all_of(columnas_a_mantener))
    
    # Conversiones de tipos 
    if ("Sales Doc." %in% names(datos)) {
      datos$`Sales Doc.` <- as.numeric(datos$`Sales Doc.`)
    }
    
    if ("EAN/UPC" %in% names(datos)) {
      datos$`EAN/UPC` <- as.numeric(datos$`EAN/UPC`)
    }
    
    if ("Ordered QTY" %in% names(datos)) {
      datos$`Ordered QTY` <- as.numeric(datos$`Ordered QTY`)
    }
    
    if ("Allocated Qty" %in% names(datos)) {
      datos$`Allocated Qty` <- as.numeric(datos$`Allocated Qty`)
    }
    
    if ("Created On" %in% names(datos)) {
      datos$`Created On` <- as.Date(as.numeric(datos$`Created On`), origin = "1899-12-30")
    }
    
    datos$archivo_origen <- basename(cArchivo)
    
    return(datos)
    
  }, error = function(e) {
    warning("Error al leer el archivo ", cArchivo, ": ", e$message)
    return(NULL)
  })
}


#============== Paths ==============
{
  #Path de usuario
  rUser <- fGetUserPath()
  
  #Path de SharePoint
  #1 <- Desarrollador
  #0 <- Despliegue
  Modo <- 0
  
  rSharePoint <- ifelse(Modo == 1, file.path("Documents", "Development", "REPORTES"), file.path("Luxottica Group S.p.A/PVC_EL_MX - Documentos"))
  
  #Path de la carpeta tablas
  rTablas <- file.path(rUser, rSharePoint, "Reportes" , "Forecast_Luxottica", "01_Tablas")
  
  #Path de la carpeta reportes
  rReportes <- file.path(rUser, rSharePoint, "Reportes", "Forecast_Luxottica", "02_Reportes")
  
  #Path de Qlik
  rQlik_PVC <- file.path("B:", "Ventas+Vision", "1_Produccion", "3_Datos_Transformados", "PVC")
  
  #Path de Qlik Supply
  rQlik_Supply <- file.path("B:", "Ventas+Vision", "1_Produccion", "3_Datos_Transformados", "SupplyChain")
  
  #Path FBEM Lux
  rFBEM_Lux <- file.path(rUser, rSharePoint, "Reportes", "FBEM", "002_FMT_LUX")
  
  #Path FBEM Sgi
  rFBEM_Sgi <- file.path(rUser, rSharePoint, "Reportes", "FBEM", "007_FMT_SGI")
  
  #Path FBEM Mv
  rFBEM_Mv <- file.path(rUser, rSharePoint, "Reportes", "FBEM", "011_FMT_MV")
  
  #================ Constantes ================ 
  #Año 
  cAnio <- substring((Sys.time()-3), 1, 4)
  
  #Mes
  cMes <- substring((Sys.time()-3), 6, 7)
  
  #Semana
  cSemana <- as.numeric(strftime(today(), format("%V")))
  
  #Día
  cDia <- substring((Sys.time()-3), 9, 10)
  
  #Primer día del mes, segun la fecha actual
  cPrimerDiaMes <- seq(as.Date(ceiling_date(Sys.Date(), "month")), length = 2, by = "-1 months")[2] 
  
  #Fecha (n) meses de venta
  cMeses <- 2
  cFechaMeses <- floor_date((today() - 1) %m+% months(- cMeses), "month")
  
  #Fecha (n) semanas atras
  cSemanas <- 8
  cFechaSemanas <- (today() -1) %m+% weeks(-cSemanas)
  
  #================ Catalogos ================ 
  #Articulo de Catalogo
  tArt_Cat <- read.csv(file.path(rQlik_PVC, "ART_CAT.csv"), header = TRUE, sep = ",") %>% 
    rename_all(toupper) %>% 
    filter(ID_GENERICO == "A") %>% #Filtramos puro Armazon
    rename(SKU = 1) %>%
    rename(TIPO = ID_TIPO) %>% 
    mutate(EAN = fRight(EAN, 13)) %>% 
    arrange(desc(SKU)) %>%
    select(SKU, TIPO, ID_LINEA, PACK, EAN, MARCA, ID_PROVEEDOR, ID_GENERICO) %>% 
    unique()
  
  #Pedidos Pendientes
  tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
    rename_all(toupper) %>% 
    mutate(FECHA_CAP = as.Date(FECHA_CAP, format = "%d/%m/%Y")) %>% 
    mutate(FECHA_PROMESA = as.Date(FECHA_PROMESA, format = "%d/%m/%Y")) %>% 
    mutate(EAN = fRight(EAN, 13))
  
  #Elementos a mantener en el environment
  vMantener <- c("vMantener")
  vMantener <- c(vMantener, ls())
  
}

#================ Ejecucion ===================
#Inventarios
source("010_Inventarios.R")

#Backorder (Pedidos Pendientes)
source("020_BackOrder.R")

#Ventas
source("030_Ventas.R")

#Stock Seguridad
source("060_Stock_Seguridad.R")

#Picking
source("070_Picking.R")

#Ejecuta si es dia Lunes
if (wday(today()) == 2) {
  
  #Necesidad
  source("040_Necesidad.R")
  
  #Requerimiento
  source("050_Requerimiento.R")
  
}


rm(list = ls())
