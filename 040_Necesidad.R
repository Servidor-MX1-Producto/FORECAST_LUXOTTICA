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
  tForecastEst[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")],
  tStock_Seguridad[,c("ID_EMPRESA", "ID_LINEA", "PACK", "TIPO")]) %>% 
  unique() %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Inventario
q000Inv <- tInventario %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Pedidos Pendientes
q000PedPend <- tPed_Pendiente %>% 
  filter(ANIO == as.numeric(cAnio)) %>% 
  filter(SEMANA <= as.numeric(cSemana)) %>% #En la semana inicial tomaremos en cuenta todos los pedidos pendientes que tengamos de la semana a ejecutar y semanas atras 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  filter(PENDIENTE > 0) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Venta estimada
q000FrcstEst <- tForecastEst %>%
  filter(ANIO == as.numeric(cAnio)) %>% 
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(FORECAST = sum(FORECAST)) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Facing Estimado
q000FacingEst <- tFacingEst %>% 
  filter(ANIO == as.numeric(cAnio)) %>% 
  filter(SEMANA == as.numeric(cSemana)) %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(FACING = sum(FACING)) %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Stock de Seguridad
q000StockSeg <- tStock_Seguridad %>% 
  mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruce de informacion
q000CruceInfo <- tDataFrameCons %>% 
  left_join(q000Inv[,c("ID_ELPT", "INVENTARIO")], by = "ID_ELPT") %>% 
  left_join(q000PedPend[,c("ID_ELPT", "PENDIENTE")], by = "ID_ELPT") %>% 
  left_join(q000FrcstEst[,c("ID_ELPT", "FORECAST")], by = "ID_ELPT") %>% 
  left_join(q000FacingEst[,c("ID_ELPT", "FACING")], by = "ID_ELPT") %>% 
  left_join(q000StockSeg[,c("ID_ELPT", "STOCK_SEGURIDAD")], by = "ID_ELPT") %>% 
  mutate_at(c("INVENTARIO", "PENDIENTE", "FORECAST", "FACING", "STOCK_SEGURIDAD"), ~replace(., is.na(.), 0)) 

#Calculos
q001Ncsd <- q000CruceInfo %>% 
  mutate(INV_F = INVENTARIO + PENDIENTE - FORECAST) %>% #Inventario Final
  mutate(INV_F = ifelse(INV_F < 0, 0, INV_F)) %>% 
  mutate(INV_I = FACING + STOCK_SEGURIDAD) %>% #Inventario de Seguridad
  mutate(NECESIDAD = INV_I - INV_F) %>% 
  mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
  mutate(ANIO = cAnio) %>% 
  mutate(SEMANA = cSemana) %>% 
  arrange(desc(ID_EMPRESA), ID_LINEA, PACK, TIPO) %>% 
  select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INVENTARIO, PENDIENTE, FORECAST, STOCK_SEGURIDAD, FACING, INV_F, INV_I, NECESIDAD, SEMANA, ANIO)

#Dataframe donde se ira agregando las necesidades por semana
q004Ncsd <- q001Ncsd

#Simulacion
#Semanas de visibilidad
cSemanasVis <- 4

#Semanas a simular
cSemanasSim <- max(unique(tLead_Time$LEAD_TIME_W)) + cSemanasVis #(Considerar LEAD time por proveedor (Lead_Time + 4))

#Bucle
#n <- 2
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
    filter(ANIO == as.numeric(cAnioCiclo)) %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(PENDIENTE = sum(PENDIENTE)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Forecast
  q002FrcstEst<- tForecastEst %>% 
    filter(ANIO == as.numeric(cAnioCiclo)) %>% 
    filter(SEMANA == as.numeric(cSemanaCiclo)) %>% 
    group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
    summarise(FORECAST = sum(FORECAST)) %>% 
    mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))
  
  #Facing 
  q002FacingEst <- tFacingEst %>% 
    filter(ANIO == as.numeric(cAnioCiclo)) %>% 
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
    left_join(q000StockSeg[,c("ID_ELPT", "STOCK_SEGURIDAD")], by = "ID_ELPT") %>% 
    mutate_at(c("INVENTARIO", "PENDIENTE", "FORECAST", "FACING", "STOCK_SEGURIDAD"), ~replace(., is.na(.), 0)) 
  
  #Calculos
  q001Ncsd <- q002CruceInfo %>% 
    mutate(INV_F = INVENTARIO + PENDIENTE - FORECAST) %>% #Inventario Final
    mutate(INV_F = ifelse(INV_F < 0, 0, INV_F)) %>% 
    mutate(INV_I = FACING + STOCK_SEGURIDAD) %>% 
    mutate(NECESIDAD = INV_I - INV_F) %>% 
    mutate(NECESIDAD = ifelse(NECESIDAD < 0, 0, NECESIDAD)) %>% 
    mutate(SEMANA = cSemanaCiclo) %>% 
    mutate(ANIO = cAnioCiclo) %>% 
    arrange(desc(ID_EMPRESA), ID_LINEA, PACK, TIPO) %>% 
    select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INVENTARIO, PENDIENTE, FORECAST, STOCK_SEGURIDAD, FACING, INV_F, INV_I, NECESIDAD, SEMANA, ANIO)
  
  #Agregamos al datframe Consolidado
  q004Ncsd <- q004Ncsd %>% 
    rbind(q001Ncsd)
  
  
  
}

#Limitar semanas a Lead Time

#Agregamos Lead Time por poveedor o por LPT (Linea Pack Tipo)
q004NcsLT <- q004Ncsd %>% 
  mutate(ID_LPT = paste(ID_LINEA, PACK, TIPO, sep = "|")) %>% 
  mutate(ID_PROVEEDOR = "LUM") %>% 
  left_join(q000LTProv[,c("ID_PROVEEDOR", "LEAD_TIME_W")], by = "ID_PROVEEDOR") %>% #Left join para saber las lineas que se contemplan por proveedor
  mutate(LEAD_TIME_W = ifelse(ID_LPT %in% q000LTLinea$ID_LPT, q000LTLinea$LEAD_TIME_W, LEAD_TIME_W)) %>% #Condicional para saber Lead Time de lineas y proveedor en especifico
  mutate_at(c("LEAD_TIME_W"), ~replace(., is.na(.), 0)) 
  
#Delimita las semanas de la necesidad respecto al Lead Time mas las semanas de visibilidad
tNecesidad <- q004NcsLT %>% 
  arrange(ID_EMPRESA, ID_LINEA, PACK, TIPO, ANIO, SEMANA) %>% 
  mutate(X = ISOweek2date(sprintf("%d-W%02d-1", as.numeric(ANIO), as.numeric(SEMANA)))) %>% 
  mutate(Y = ISOweek2date(sprintf("%d-W%02d-1", as.numeric(cAnio), as.numeric(cSemana))) + weeks(LEAD_TIME_W + cSemanasVis)) %>% 
  filter(X <= Y)
  mutate(LEAD_TIME_W2 = LEAD_TIME_W + cSemana + cSemanasVis) #%>%  #Sumamos al Lead Time la semana en la que estamos y las semanas de visibilidad
  filter(SEMANA <= LEAD_TIME_W) %>% 
  arrange(ID_EMPRESA, ID_LINEA, PACK, TIPO, SEMANA)

#Escribe reporte
#write.csv(tNecesidad, file.path(rReportes, "NCSD.csv"), row.names = FALSE)

#Lista de data frames a conservar
vGuarda <- c("tNecesidad", "cSemanasVis") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)


















