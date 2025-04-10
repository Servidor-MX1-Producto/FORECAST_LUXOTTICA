#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Facing
tFacingEst <- read.csv(file.path(rTablas, "FACING.csv"), header = TRUE, sep = ",") 

#Sell Out
tForecastEst <- read.csv(file.path(rTablas, "FORECAST.csv"), header = TRUE, sep = ",") 

#================ Ejecucion ===================
#Define Facing a nivel semana
tFacing <- tFacingEst %>% 
  left_join(tCalendario[,c("SEMANA", "MES")], by = "MES")

#Define Venta a nivel semana 
tSellOut <- tSellOutEst %>% 
  left_join(tCalendario[,c("SEMANA", "MES")], by = "MES") %>% 
  mutate(VENTA = ceiling(VENTA / 4))

#Lista de data frames a conservar
vGuarda <- c("tFacing", "tSellOut") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
 