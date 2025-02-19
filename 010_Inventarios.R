#================ About ================
# Pasos de ejecución:
#   1.- Extraer información de Qlik
#   2.- Calcular Cantidad y Volumen
#   3.- Exportar resultados

#================ Importaciones ===================
#Inventario Lux
tInv_Lux <- read.csv(file.path(rFBEM_Lux, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(BANNER = "LUX")

#Inventario Sgi
tInv_Sgi <- read.csv(file.path(rFBEM_Sgi, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(BANNER = "SGI")

#Inventario Mv
tInv_Mv <- read.csv(file.path(rFBEM_Mv, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(BANNER = "MV")

#Sucursales a considerar
tSucursales <- read.csv(file.path(rTablas, "Sucursales.csv"), header = TRUE, sep = ",")%>% 
  rename_all(toupper)

#================ Ejecucion ===================
#Consolida inventarios
q001ConsolidaInv <- rbind(
  tInv_Lux[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "BANNER")],
  tInv_Sgi[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "BANNER")],
  tInv_Mv[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "BANNER")]
)

#Cruzamos informacion de Sucursales y de Skus
q002CruceInf <- q001ConsolidaInv %>% 
  left_join(tSucursales[,c("ID_ALMACEN", "CONSIDERA")], by = "ID_ALMACEN") %>% #Cruce con Sucursales
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") #Cruce con Skus

#Agrupacion de la informacion
tInventario <- q002CruceInf %>% 
  filter(EB_EL_3P == "EL") %>% #Filtramos Luxottica
  filter(CONSIDERA == "Si") %>% #Filtramos sucursales que si se consideran
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% #Tip: segmentar por cadena el agrupamiento MV: Pack|Tipo LUX:Linea|Tipo SGI:Linea
  summarise(INVENTARIO = sum(EXISTENCIA)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))


#Lista de data frames a conservar
vGuarda <- c("tInventario") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
