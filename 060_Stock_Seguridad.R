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
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#================ Ejecucion ===================
#Creando dataframe base
q000DataFrameBase <- tForecastEst[,c("ID_EMPRESA", "ANIO", "SEMANA", "ID_LINEA", "PACK", "TIPO")] %>% 
  unique() %>% 
  filter(ANIO == cAnio) %>% 
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruzando Ventas
#Preparando ventas
q001Vta <- tVenta %>% 
  filter(ANIO == cAnio) %>% 
  arrange(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, VENTA) %>% 
  mutate(ID_EASLPT = paste(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, sep = "|"))

#Cruce de la venta y del Forecast
q001CrossVentas <- q000DataFrameBase %>% 
  left_join(q001Vta[,c("ID_EASLPT", "VENTA")], by = "ID_EASLPT") %>% 
  left_join(tForecastEst[,c("ID_EASLPT", "FORECAST")], by = "ID_EASLPT") %>% 
  mutate_at(c("VENTA", "FORECAST"), ~replace(., is.na(.), 0)) #%>% 
  filter(SEMANA < cSemana)

#Calculando Error
q002ErrorCalc <- q001CrossVentas %>% 
  mutate(ERROR = VENTA - FORECAST)

#Calculando Desviacion Estandar
q003DesvEstdr <- q002ErrorCalc %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  summarise(
    DESV_ERROR = sd(ERROR),
    .groups = "drop"
  )

#Calculando Stock de Seguridad
#Nivel de Servicio
Z <- 1.65
#Z <- qnorm(0.95)

#Calculo del Stock de seguridad
q004StockSeg <- q003DesvEstdr %>% 
  mutate(STOCK_SEGURIDAD = round(Z * DESV_ERROR, 0)) %>% 
  mutate(ID = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|"))


x <- tForecastEst %>% 
  mutate(ID = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, sep = "|")) %>% 
  left_join(q004StockSeg[,c("ID", "STOCK_SEGURIDAD")], by = "ID")
  




