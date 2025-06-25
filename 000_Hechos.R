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
  if (!require("tidyr")) install.packages("tidyr")
  library("tidyr")
  if (!require("rio")) install.packages("rio")
  library("rio")
  if (!require("writexl")) install.packages("writexl")
  library("writexl")
  
  #Zona Horaria
  Sys.setenv(TZ = "Etc/GMT+6")
}

{
  
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
  
  
  #============== Paths ==============
  #Path de usuario
  rUser <- fGetUserPath()
  
  #Path de SharePoint
  #1 <- Desarrollador
  #0 <- Despliegue
  Modo <- 0
  
  rSharePoint <- ifelse(Modo == 1, file.path("Documents", "Development", "REPORTES"), file.path("GrandVision/MX1-MV Supply Chain - Documentos"))
  
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
#source("060_Stock_Seguridad.R")

#Necesidad
source("040_Necesidad.R")

#Requerimiento
source("050_Requerimiento.R")

rm(list = ls())
