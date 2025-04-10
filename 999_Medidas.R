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
  unique() 

#Pedidos Pendientes
q000PedPend <- tPed_Pendiente %>% 
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Venta de la semana
q000Venta <- tSellOut %>% 
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(VENTA = sum(VENTA)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Venta estimada
q000VtaEst <- tSellOut %>%
  #filter(SEMANA == as.numeric(strftime(today() + days(21), format("%V")))) %>% #Filtramos la semana mas lead time
  filter(SEMANA == as.numeric(cSemana) + 1) %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>%
  summarise(VTA_ESTIMADA = sum(VENTA)) %>%
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Facing Estimado
q000FacingEst <- tFacing %>% 
  #filter(SEMANA == as.numeric(strftime(today() + days(21), format("%V")))) %>% #Filtramos la semana mas lead time
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(FACING_ESTIMADO = sum(FACING)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Cruce de informacion
q000CruceInfo <- tDataFrameCons %>% 
  left_join(tInventario[,c("ID_BFPL", "INVENTARIO")], by = "ID_BFPL") %>% 
  left_join(q000PedPend[,c("ID_BFPL", "PENDIENTE")], by = "ID_BFPL") %>% 
  left_join(q000Venta[,c("ID_BFPL", "VENTA")], by = "ID_BFPL") %>% 
  left_join(q000VtaEst[,c("ID_BFPL", "VTA_ESTIMADA")], by = "ID_BFPL") %>% 
  left_join(q000FacingEst[,c("ID_BFPL", "FACING_ESTIMADO")], by = "ID_BFPL") %>% 
  mutate_at(c("INVENTARIO", "PENDIENTE", "VENTA", "VTA_ESTIMADA", "FACING_ESTIMADO"), ~replace(., is.na(.), 0)) 

#Calculos
q001Ncsd <- q000CruceInfo %>% 
  mutate(INV_F = INVENTARIO + PENDIENTE - VENTA) %>% #Inventario Final
  mutate(INV_I = FACING_ESTIMADO + VTA_ESTIMADA) %>% 
  mutate(NECESIDAD = INV_I - INV_F) %>% 
  mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, INVENTARIO, PENDIENTE, VENTA, VTA_ESTIMADA, FACING_ESTIMADO, INV_I, INV_F, NECESIDAD)


#Escribe reporte de n semanas adelante; n = lead time
cNombreArchivo <- paste("NCSD_", "W", strftime(today() + days(21), format("%V")), ".csv", sep = "")

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
    select(BANNER, FRAMES_SUN, PACK, ID_LINEA, NECESIDAD) %>% 
    rename(INVENTARIO = NECESIDAD) %>% 
    group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
    summarise(INVENTARIO = sum(INVENTARIO)) %>% 
    mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
  #Pedidos pendientes
  q002PedPend <- tPed_Pendiente %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
    summarise(PENDIENTE = sum(PENDIENTE)) %>% 
    mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
  #Venta de la semana
  q002Venta <- tSellOut %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
    summarise(VENTA = sum(VENTA)) %>% 
    mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
  #Venta estimada
  q002VtaEst <- tSellOut %>%
    #filter(SEMANA == as.numeric(strftime(today() + days(21), format("%V")))) %>% #Filtramos la semana mas lead time
    filter(SEMANA == as.numeric(cSemanaCiclo) + 1) %>% 
    group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>%
    summarise(VTA_ESTIMADA = sum(VENTA)) %>%
    mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
  #Facing Estimado
  q002FacingEst <- tFacing %>% 
    #filter(SEMANA == as.numeric(strftime(today() + days(21), format("%V")))) %>% #Filtramos la semana mas lead time
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
    summarise(FACING_ESTIMADO = sum(FACING)) %>% 
    mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
  #Cruce de informacion
  q002CruceInfo <- tDataFrameCons %>% 
    left_join(q002Inv[,c("ID_BFPL", "INVENTARIO")], by = "ID_BFPL") %>% 
    left_join(q002PedPend[,c("ID_BFPL", "PENDIENTE")], by = "ID_BFPL") %>% 
    left_join(q002Venta[,c("ID_BFPL", "VENTA")], by = "ID_BFPL") %>% 
    left_join(q002VtaEst[,c("ID_BFPL", "VTA_ESTIMADA")], by = "ID_BFPL") %>% 
    left_join(q002FacingEst[,c("ID_BFPL", "FACING_ESTIMADO")], by = "ID_BFPL") %>% 
    mutate_at(c("INVENTARIO", "PENDIENTE", "VENTA", "VTA_ESTIMADA", "FACING_ESTIMADO"), ~replace(., is.na(.), 0)) 
  
  #Calculos
  q001Ncsd <- q002CruceInfo %>% 
    mutate(INV_F = INVENTARIO + PENDIENTE - VENTA) %>% #Inventario Final
    mutate(INV_I = FACING_ESTIMADO + VTA_ESTIMADA) %>% 
    mutate(NECESIDAD = INV_I - INV_F) %>% 
    mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
    select(BANNER, FRAMES_SUN, PACK, ID_LINEA, INVENTARIO, PENDIENTE, VENTA, VTA_ESTIMADA, FACING_ESTIMADO, INV_I, INV_F, NECESIDAD)
  
  #Escribe reporte de n semanas adelante; n = lead time
  cNombreArchivo <- paste("NCSD_SIM_", "W", cSemanaCiclo, ".csv", sep = "")
  
  #write.csv(q001Ncsd, file.path(rReportes, cAnio, cNombreArchivo), row.names = FALSE)
  
}























