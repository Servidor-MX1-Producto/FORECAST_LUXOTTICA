#================ About ================
# Pasos de ejecución:
#   1.- Extraer información de Qlik
#   2.- Calcular Cantidad y Volumen
#   3.- Exportar resultados

#================ Imortaciones ===================

#articulo de Catalogo
tArt_Cat <- read.csv(file.path(rQlik_PVC, "ART_CAT.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  rename(SKU = 1) %>% 
  mutate(EAN = fRight(EAN,13)) %>% 
  arrange(desc(SKU)) %>%
  select(SKU, ID_TIPO, ID_LINEA, ID_COLOR, POLARIZADO, GRADUABLE, GENERO, EAN, MODELO, EB_NEB, MARCA, ID_PROVEEDOR, PRECIO_L1, COSTO, ID_GENERICO) %>% 
  unique()

#================ Ejecucion ===================
#Inventario
#Path de Proyecto Luxottica
rLuxottica <- file.path(rUser, rSharePoint, "Luxottica", "02_Reportes", "SEDICO", cAnio)

#Leer archivos de la carpeta
q000_Lista_Archivos <- list.files(file.path(rLuxottica), pattern = "") %>% 
  data.frame() %>% 
  rename(ARCHIVO = 1) %>% 
  mutate(RUTA = paste(rLuxottica, "/", ARCHIVO, sep = "")) %>% 
  mutate(FECHA_CREACION = as.Date(file.mtime(RUTA))) %>% 
  arrange(desc(FECHA_CREACION))

#Leer archivo mas reciente
q001_Inv_SEDICO <- read_excel(file.path(q000_Lista_Archivos$RUTA[1])) %>% 
  rename_all(toupper) %>% 
  mutate(EAN = fRight(UPC, 13)) %>% 
  left_join(tArt_Cat[,c("EAN", "ID_LINEA", "ID_TIPO", "SKU")], by = "EAN") %>% 
  rename(EXISTENCIA = "AVAILABLE STOCK") %>% 
  arrange(ID_LINEA, ID_TIPO, desc(EXISTENCIA)) %>% 
  data.frame() %>% 
  select(SKU, ID_LINEA, ID_TIPO, EXISTENCIA)
  

#Escritura
write.csv(q001_Inv_SEDICO, file.path(rTablas, "Inventario.csv"), row.names = FALSE)


