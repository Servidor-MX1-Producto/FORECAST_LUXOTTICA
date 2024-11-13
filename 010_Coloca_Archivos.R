#================ About ================
# Pasos de ejecución:
#   1.- Extraer información de Qlik
#   2.- Calcular Cantidad y Volumen
#   3.- Exportar resultados

#================ Imortaciones ===================

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
q001_Inv_SEDICO <- read_excel(file.path(q000_Lista_Archivos$RUTA[1]))

write.csv(file.path(rTablas, "Inventario.csv"), header = TRUE, sep = ",")


