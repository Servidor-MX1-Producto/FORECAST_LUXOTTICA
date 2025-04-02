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
  mutate(ANIO = year(FECHA)) %>% 
  mutate(BANNER = case_when(
    ID_EMPRESA == 7 ~ "SGI",
    ID_EMPRESA == 2 ~ "LUX",
    ID_EMPRESA == 11 ~ "MV",
    TRUE ~ NA_character_  # Corrección: NA explícito para tipo character  # En caso de que haya otros valores, los mantenemos como están
  )) %>% 
  select(FECHA, ANIO, MES, SEMANA, BANNER, ID_ALMACEN, SKU, CANT)

tVenta <- q000VentasClean %>% 
  left_join(tArt_Cat[,c("SKU", "FRAMES_SUN", "ID_LINEA", "PACK", "EB_EL_3P", "MARCA")], by = "SKU") %>% 
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  filter(ANIO == as.integer(cAnio)) %>% #Filtramos el anio en el que ejecutamos el proceso
  filter(SEMANA == as.integer(cSemana)) %>% #Filtramos el mes en el que ejecutamos el proceso
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(VENTA = sum(CANT)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  
#Lista de data frames a conservar
vGuarda <- c("tVenta") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
