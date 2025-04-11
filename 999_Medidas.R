#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Facing 
tFacingEst <- read.csv(file.path(rTablas, "FACING.csv"), header = TRUE, sep = ",") %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ANIO = as.integer(ANIO),
    SEMANA = as.integer(SEMANA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    FACING = as.integer(FACING))

#Forecast
tForecastEst <- read.csv(file.path(rTablas, "FORECAST.csv"), header = TRUE, sep = ",") %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ANIO = as.integer(ANIO),
    SEMANA = as.integer(SEMANA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    FORECAST = as.integer(FORECAST)) 

#================ Ejecucion ===================
#DataFrame Base
tDataFrameCons <- rbind(
  tInventario[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")],
  tPed_Pendiente[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")],
  tVenta[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")],
  tFacingEst[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")],
  tForecastEst[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")]) %>% 
  unique() %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Inventario
q000Inv <- tInventario %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Pedidos Pendientes
q000PedPend <- tPed_Pendiente %>% 
  filter(SEMANA <= as.numeric(cSemana)) %>% #En la semana inicial tomaremos en cuenta todos los pedidos pendientes que tengamos de la semana a ejecutar y semanas atras 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Venta estimada
q000FrcstEst <- tForecastEst %>%
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(FORECAST = sum(FORECAST)) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Facing Estimado
q000FacingEst <- tFacingEst %>% 
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(FACING = sum(FACING)) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruce de informacion
q000CruceInfo <- tDataFrameCons %>% 
  left_join(q000Inv[,c("ID_ELPT", "INVENTARIO")], by = "ID_ELPT") %>% 
  left_join(q000PedPend[,c("ID_ELPT", "PENDIENTE")], by = "ID_ELPT") %>% 
  left_join(q000FrcstEst[,c("ID_ELPT", "FORECAST")], by = "ID_ELPT") %>% 
  left_join(q000FacingEst[,c("ID_ELPT", "FACING")], by = "ID_ELPT") %>% 
  mutate_at(c("INVENTARIO", "PENDIENTE", "FORECAST", "FACING"), ~replace(., is.na(.), 0)) 

#Calculos
q001Ncsd <- q000CruceInfo %>% 
  mutate(INV_F = INVENTARIO + PENDIENTE - FORECAST) %>% #Inventario Final
  mutate(INV_F = ifelse(INV_F < 0, 0, INV_F)) %>% 
  mutate(INV_I = FACING + FORECAST) %>% 
  mutate(NECESIDAD = INV_I - INV_F) %>% 
  mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
  arrange(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INVENTARIO, PENDIENTE, FORECAST, FACING, INV_F, INV_I, NECESIDAD)


#Escribe reporte de n semanas adelante; n = lead time
cNombreArchivo <- paste("NCSD_", "W", cSemana, ".csv", sep = "")

write.csv(q001Ncsd, file.path(rReportes, cAnio, cNombreArchivo), row.names = FALSE)

#Simulacion
cSemanasSim <- 8

#Bucle
n <- 1
for (n in 1:cSemanasSim) {
  
  #Sumamos fechas a la actual dependiendo el ciclo de la semanas
  cFechaCiclo <- today() + days(n * 7) #Multiplicamos el ciclo del bucle por 7 para interpretar los dias
  cSemanaCiclo <- as.numeric(strftime(cFechaCiclo, format("%V")))
  cMesCiclo <- substring(cFechaCiclo, 6, 7)
  cAnioCiclo <- substring(cFechaCiclo, 1, 4)
  
  #Inventario
  q002Inv <- q001Ncsd %>% 
    select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INV_F) %>% 
    rename(INVENTARIO = INV_F) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(INVENTARIO = sum(INVENTARIO)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Pedidos pendientes
  q002PedPend <- tPed_Pendiente %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(PENDIENTE = sum(PENDIENTE)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Forecast
  q002FrcstEst<- tForecastEst %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(FORECAST = sum(FORECAST)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Facing 
  q002FacingEst <- tFacingEst %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(FACING = sum(FACING)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Cruce de informacion
  q002CruceInfo <- tDataFrameCons %>% 
    left_join(q002Inv[,c("ID_ELPT", "INVENTARIO")], by = "ID_ELPT") %>% 
    left_join(q002PedPend[,c("ID_ELPT", "PENDIENTE")], by = "ID_ELPT") %>% 
    left_join(q002FrcstEst[,c("ID_ELPT", "FORECAST")], by = "ID_ELPT") %>% 
    left_join(q002FacingEst[,c("ID_ELPT", "FACING")], by = "ID_ELPT") %>% 
    mutate_at(c("INVENTARIO", "PENDIENTE", "FORECAST", "FACING"), ~replace(., is.na(.), 0)) 
  
  #Calculos
  q001Ncsd <- q002CruceInfo %>% 
    mutate(INV_F = INVENTARIO + PENDIENTE - FORECAST) %>% #Inventario Final
    mutate(INV_I = FACING + FORECAST) %>% 
    mutate(INV_F = ifelse(INV_F < 0, 0, INV_F)) %>% 
    mutate(NECESIDAD = INV_I - INV_F) %>% 
    mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
    select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INVENTARIO, PENDIENTE, FORECAST, FACING, INV_F, INV_I, NECESIDAD)
  
  #Escribe reporte de n semanas adelante; n = lead time
  cNombreArchivo <- paste("NCSD_SIM_", "W", cSemanaCiclo, ".csv", sep = "")
  
  #write.csv(q001Ncsd, file.path(rReportes, cAnio, cNombreArchivo), row.names = FALSE)
  
}























