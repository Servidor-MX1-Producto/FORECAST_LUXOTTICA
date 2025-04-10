#================ About ================
# Pasos de ejecución:
#   Pedidos Pendientes
#     1.- 
#     2.- 
#     3.- 
#     4.- 

#================ Importaciones ===================
#Pedidos Pendientes
tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) 

#================ Ejecucion ===================
#Limpieza Pedidos Pendientes
q001PPendientesClean <- tPedidos_Pendientes %>% 
  mutate(EAN = fRight(EAN, 13)) %>%  #Agregar validador de estados
  mutate(FECHA = as.Date(FECHA_PROMESA, format("%d/%m/%Y"))) %>% 
  mutate(SEMANA = as.numeric(strftime(FECHA, format("%V")))) %>% #Define semana
  mutate(MES = month(FECHA)) %>% #Define mes
  mutate(ANIO = year(FECHA)) %>% #Define anio  
  mutate(ID_EMPRESA = case_when(
      CEDIS == "NARANJO" ~ "11",
      CEDIS == "PINO" ~ "2",
      CEDIS == "CANCUN" ~ "7",
      TRUE ~ NA_character_  # Corrección: NA explícito para tipo character  # En caso de que haya otros valores, los mantenemos como están
    )) %>% 
  rename(SHIPMENT = STARS) %>% 
  select(ID_PEDIDO, SHIPMENT, CANTIDAD, ENTREGADO, PENDIENTE, SKU, EAN, ESTADO, PEDIDO_PEND, ID_EMPRESA, FECHA, ANIO, MES, SEMANA)

#Cruza y filtra Informacion 
q001InfoFormat <- q001PPendientesClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "TIPO", "ID_PROVEEDOR")], by = "SKU") %>% #Cruce con catalogo de Articulos
  filter(ID_PROVEEDOR == "LUM") %>% #Filtra Tipo de Marca
  filter(PEDIDO_PEND == "Y") %>% #Filtro pedido pendiente
  select(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, PENDIENTE)


#Checar fecha promesa, en caso de no tener olocar fecha promesa de 3 semanas 



#Casteo de valores en dataframe final
tPed_Pendiente <- q001InfoFormat %>% 
  mutate(
    ID_EMPRESA = as.character(ID_EMPRESA),
    ANIO = as.integer(ANIO),
    SEMANA = as.integer(SEMANA),
    ID_LINEA = as.character(ID_LINEA),
    PACK = as.character(PACK),
    TIPO = as.character(TIPO),
    PENDIENTE = as.integer(PENDIENTE))

#Lista de data frames a conservar
vGuarda <- c("tPed_Pendiente") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
