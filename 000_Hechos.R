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
    #============== Paths ==============
    #Path de usuario
    rUser <- fGetUserPath()
    
    #Path de SharePoint
    #1 <- Desarrollador
    #0 <- Despliegue
    Modo <- 1
    
    rSharePoint <- ifelse(Modo == 1, file.path("Documents", "Development", "REPORTES"), file.path("GrandVision/MX1-MV Supply Chain - Documentos", "Reportes"))
    
    #Path de la carpeta tablas
    rTablas <- file.path(rUser, rSharePoint, "Forecast_Luxottica", "01_Tablas")
    
    #Path de la carpeta reportes
    rReportes <- file.path(rUser, rSharePoint, "Forecast_Luxottica", "02_Reportes")
    
    #Path de Qlik
    rQlik_PVC <- file.path("B:", "Ventas+Vision", "1_Produccion", "3_Datos_Transformados", "PVC")
    
    #Path de Qlik Supply
    rQlik_Supply <- file.path("B:", "Ventas+Vision", "1_Produccion", "3_Datos_Transformados", "SupplyChain")
    
    #================ Constantes ================ 
    
    #Año 
    cAnio <- substring((Sys.time()-3), 1, 4)
    
    #Mes
    cMes <- substring((Sys.time()-3), 6, 7)
    
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
    
    #Elementos a mantener en el environment
    vMantener <- c("vMantener")
    vMantener <- c(vMantener, ls())
    
}

#================ Ejecucion ===================
#Surte Almacen Mas Vision
source("010_Coloca_Archivos.R")



rm(list = ls())