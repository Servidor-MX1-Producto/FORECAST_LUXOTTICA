#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Inventario Lux
tInv_Lux <- read.csv(file.path(rFBEM_Lux, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(ID_EMPRESA = 2)

#Inventario Sgi
tInv_Sgi <- read.csv(file.path(rFBEM_Sgi, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(ID_EMPRESA = 7)

#Inventario Mv
tInv_Mv <- read.csv(file.path(rFBEM_Mv, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(ID_EMPRESA = 11)

#Sucursales a considerar
tSucursales_Considerar <- read.csv(file.path(rTablas, "SUCURSALES_CONSIDERA.csv"), header = TRUE, sep = ",")%>% 
  rename_all(toupper)

#================ Ejecucion ===================
#Consolida inventarios
q001ConsolidaInv <- rbind(
  tInv_Lux[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "ID_EMPRESA")],
  tInv_Sgi[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "ID_EMPRESA")],
  tInv_Mv[,c("ID_ALMACEN", "SKU", "EXISTENCIA", "ID_EMPRESA")]
)

#Cruzamos informacion de Sucursales y de Skus
q002CruceInf <- q001ConsolidaInv %>% 
  left_join(tSucursales_Considerar[,c("ID_ALMACEN", "CONSIDERA")], by = "ID_ALMACEN") %>% #Cruce con Sucursales
  mutate_at(c("CONSIDERA"),~replace(.,is.na(.), "SI")) %>% #Rellena los vacion con SI para contemplar sucursales
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "TIPO", "ID_PROVEEDOR")], by = "SKU") %>% #Cruce con catalogo de articulos
  mutate(PACK = ifelse(ID_EMPRESA != c(11) | is.na(PACK) | nchar(PACK) == 0, "-", PACK)) #Solo se considera Pack para Mas Vision
  
#Agrupacion de la informacion
q002InfoFormat <- q002CruceInf %>% 
  filter(ID_PROVEEDOR == "LUM") %>% #Filtramos Luxottica
  filter(CONSIDERA != "NO") %>% #Filtramos sucursales que NO se consideran
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% #Agrupacion
  summarise(INVENTARIO = sum(EXISTENCIA)) #%>% 
  #mutate(ID_ELPT = paste(ID_EMPRESA, ID_LINEA, PACK, ID_TIPO, sep = "|"))

#Casteo de valores en dataframe final
tInventario <- q002InfoFormat %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    INVENTARIO = as.integer(INVENTARIO))
  

#Lista de data frames a conservar
vGuarda <- c("tInventario") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
