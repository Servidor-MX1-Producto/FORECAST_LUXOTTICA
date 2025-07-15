#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Forecast
tForecastEst <- read.csv(file.path(rTablas, "FORECAST.csv"), header = TRUE, sep = ",") %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ANIO = as.integer(ANIO),
    SEMANA = as.integer(SEMANA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    FORECAST = as.integer(FORECAST)) %>% 
  filter(ANIO >= substring(cFechaSemanas, 1, 4)) %>% #Filtramos el anio de la venta de (n) Semanas atras
  filter(SEMANA >= as.numeric(strftime(cFechaSemanas, format("%V")))) %>% #Filtramos la semana de la venta de (n) Semanas atras
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#================ Ejecucion ===================
#Creando dataframe base
q000DataFrameBase <- tForecastEst[,c("ID_EMPRESA", "ANIO", "SEMANA", "ID_LINEA", "PACK", "TIPO")] %>% 
  unique() %>% 
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruzando Ventas
#Preparando ventas
q001Vta <- tVenta %>% 
  filter(ANIO >= substring(cFechaSemanas, 1, 4)) %>% #Filtramos el anio de la venta de (n) Semanas atras
  filter(SEMANA >= as.numeric(strftime(cFechaSemanas, format("%V")))) %>% #Filtramos la semana de la venta de (n) Semanas atras
  arrange(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, VENTA) %>% 
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruce de la venta y del Forecast
q001CrossVentas <- q000DataFrameBase %>% 
  left_join(q001Vta[,c("ID_EASLPT", "VENTA")], by = "ID_EASLPT") %>% 
  left_join(tForecastEst[,c("ID_EASLPT", "FORECAST")], by = "ID_EASLPT") %>% 
  mutate_at(c("VENTA", "FORECAST"), ~replace(., is.na(.), 0)) %>% 
  filter(SEMANA < cSemana) #Filtro de semana por el dato de las ventas, el cual solo traemos hasta un dia antes de la ejecucion

#Calculando Error
q002ErrorCalc <- q001CrossVentas %>% 
  mutate(ERROR = VENTA - FORECAST)

#Calculando Desviacion Estandar
q003DesvEstdr <- q002ErrorCalc %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(
    DESV_STDR = sd(ERROR),
    .groups = "drop"
  )

#Calculando Stock de Seguridad
#Nivel de Servicio
Z <- 1.65
#Z <- qnorm(0.95)

#Calculo del Stock de seguridad
tStock_Seguridad <- q003DesvEstdr %>% 
  mutate(STOCK_SEGURIDAD = round(Z * DESV_STDR, 0)) 

#Lista de data frames a conservar
vGuarda <- c("tStock_Seguridad") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
