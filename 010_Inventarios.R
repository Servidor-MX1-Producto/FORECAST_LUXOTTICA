#================ About ================
# Pasos de ejecución:
#   1.- Extraer información de Qlik
#   2.- Calcular Cantidad y Volumen
#   3.- Exportar resultados

#================ Importaciones ===================

#Articulo de Catalogo
tArt_Cat <- read.csv(file.path(rQlik_PVC, "ART_CAT.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  rename(SKU = 1) %>% 
  mutate(EAN = fRight(EAN,13)) %>% 
  mutate(
    # Usamos case_when para definir tipo de marca
    EB_EL_3P = case_when(
      ID_PROVEEDOR == "LUM" ~ "EL",    #Para Proveedor "LUM" se usa "EL"
      ID_PROVEEDOR == "GVSC" ~ "EB",   #Para Proveedor "GVSC" se usa "EB"
      TRUE ~ "3P"                      #Para todo lo demas usamos "3P" 
    )) %>% 
  mutate(
    #Usamos case_when para definir tipo de SKU
    FRAMES_SUN = case_when(
      ID_TIPO == "O" ~ "FRAMES",    #Para Tipo "O" se usa "Frames"
      TRUE ~ "SUN"                  #Para todo lo demas usamos "Sun" 
    )) %>% 
  arrange(desc(SKU)) %>%
  select(SKU, ID_TIPO, FRAMES_SUN, ID_LINEA, PACK, EAN, EB_EL_3P, MARCA, ID_PROVEEDOR, ID_GENERICO) %>% 
  unique()

#Inventario Lux
tInv_Lux <- read.csv(file.path(rFBEM_Lux, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper)

#Inventario Sgi
tInv_Sgi <- read.csv(file.path(rFBEM_Sgi, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper)

#Inventario Mv
tInv_Mv <- read.csv(file.path(rFBEM_Mv, "01_Tablas", "Armazones", "Inventario_A.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper)

#Sucursales
tSucursales <- read.csv(file.path(rTablas, "Sucursales.csv"), header = TRUE, sep = ",")%>% 
  rename_all(toupper)

#================ Ejecucion ===================
#Consolida inventarios
q001ConsolidaInv <- rbind(
  tInv_Lux[,c("ID_ALMACEN", "SKU", "EXISTENCIA")],
  tInv_Sgi[,c("ID_ALMACEN", "SKU", "EXISTENCIA")],
  tInv_Mv[,c("ID_ALMACEN", "SKU", "EXISTENCIA")]
)

#Cruzamos informacion de Sucursales y de Skus
q002CruceInf <- q001ConsolidaInv %>% 
  left_join(tSucursales[,c("ID_ALMACEN", "BANNER", "CONSIDERA")], by = "ID_ALMACEN") %>% #Cruce con Sucursales
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") #Cruce con Skus

#Agrupacion de la informacion
tInventario <- q002CruceInf %>% 
  filter(EB_EL_3P == "EL") %>% #Filtramos Luxottica
  filter(CONSIDERA == "Si") %>% #Filtramos sucursales que si se consideran
  group_by(BANNER, FRAMES_SUN, ID_LINEA, PACK) %>% #Tip: segmentar por cadena el agrupamiento MV: Pack|Tipo LUX:Linea|Tipo SGI:Linea
  summarise(EXISTENCIA = sum(EXISTENCIA))


#Lista de data frames a conservar
vGuarda <- c("tInventario", "tArt_Cat") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
