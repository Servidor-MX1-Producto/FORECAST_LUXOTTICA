#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Ventas de Armazones
tVentas_Arm <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Ventas", "Reportes", "Armazones", "Ventas_A_Cy_Ly.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper)

#Sucursales Lux
tSucursales_Lux <- read.csv(file.path(rFBEM_Lux, "01_Tablas", "Armazones", "Sucursales_LUX.csv"), header = TRUE, sep = ",")

#================ Ejecucion ===================
#Limpa tabla de ventas
q000VentasClean <- tVentas_Arm %>% 
  left_join(tSucursales_Lux[,c("ID_CC","ID_ALMACEN")], by= "ID_CC") %>% 
  mutate(ID_ALMACEN = ifelse(is.na(ID_ALMACEN), ID_CC, ID_ALMACEN)) %>% 
  mutate(FECHA = as.Date(FECHA, format("%Y-%m-%d"))) %>% 
  mutate(SEMANA = as.numeric(strftime(FECHA, format("%V")))) %>% 
  mutate(MES = month(FECHA)) %>% 
  mutate(ANIO = year(FECHA))  %>% 
  rename(VENTA = CANT) %>% 
  select(FECHA, ANIO, MES, SEMANA, ID_EMPRESA, ID_ALMACEN, SKU, VENTA)

#Cruza y filtra Informacion 
q000InfoFormat <- q000VentasClean %>% 
  left_join(tArt_Cat[,c("SKU", "TIPO", "ID_LINEA", "PACK", "ID_PROVEEDOR", "MARCA")], by = "SKU") %>% 
  filter(ID_PROVEEDOR == "LUM") %>% #Filtra Tipo de Marca
  select(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, VENTA)

#Casteo de valores en dataframe final
tVenta <- q000InfoFormat %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ANIO = as.integer(ANIO),
    SEMANA = as.integer(SEMANA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    VENTA = as.integer(VENTA))

#Lista de data frames a conservar
vGuarda <- c("tVenta") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
