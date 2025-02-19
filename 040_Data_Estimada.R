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

#================ Ejecucion ===================
tFacing <- tFacingEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, FEBRERO_2025) %>% 
  rename(FACING = FEBRERO_2025) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

tSellOut <- tSellOutEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, FEBRERO_2025) %>% 
  rename(SELLOUT = FEBRERO_2025) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))



#Lista de data frames a conservar
vGuarda <- c("tFacing", "tSellOut") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
