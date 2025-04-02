#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Facing
tFacingEst <- read.csv(file.path(rTablas, "Facing.csv"), header = TRUE, sep = ",") 

#Sell Out
tSellOutEst <- read.csv(file.path(rTablas, "Sell_Out.csv"), header = TRUE, sep = ",") 

#Tabla anual de dias del anio
tCalendario <- read.csv(file.path(rTablas, "Calendario.csv"), header = TRUE, sep = ",") %>% 
  filter(MES == as.numeric(cMes)) %>% 
  mutate(ULT_DIA = max(DIA)) %>% 
  select(SEMANA, MES, ULT_DIA) %>% 
  unique()

#================ Ejecucion ===================
#Define Facing a nivel semana
tFacing <- tFacingEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, (as.numeric(cMes) + 1) + 5) %>% #Seleccionamos el mes siguiente
  rename(FACING_ESTIMADO = 5) %>% #Renombre la columna
  mutate(MES = as.numeric(cMes)) %>% #Crea columna con el numero de mes
  left_join(tCalendario[,c("MES", "SEMANA", "ULT_DIA")], by = "MES") %>% #Cruce con calendario para traernos las semanas del mes y el numero de dias del mes
  mutate(FACING_DIA = FACING_ESTIMADO / ULT_DIA) %>%  #Dividimos le Facing (mensual) entre el numero de dias del mes
  mutate(FACING_SEMANA = ceiling(FACING_DIA * 7)) %>% #Multiplicamos el facing diario por 7 para crear el facing semanal
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, SEMANA, FACING_SEMANA) %>% 
  unique() %>% #Generamos datos unicos para que tengamos los datos a nivel semana
  rename(FACING_ESTIMADO = FACING_SEMANA) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Define Venta a nivel semana 
tSellOut <- tSellOutEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, as.numeric(cMes) + 5) %>% 
  rename(VENTA_ESTIMADA = 5) %>% 
  mutate(MES = as.numeric(cMes)) %>%
  left_join(tCalendario[,c("MES", "SEMANA", "ULT_DIA")], by = "MES") %>%
  mutate(VTA_EST_DIA = VENTA_ESTIMADA / ULT_DIA) %>% 
  mutate(VTA_EST_SEM = ceiling(VTA_EST_DIA * 7)) %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, SEMANA, VTA_EST_SEM) %>% 
  unique() %>% 
  rename(VENTA_ESTIMADA = VTA_EST_SEM) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Lista de data frames a conservar
vGuarda <- c("tFacing", "tSellOut") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
 