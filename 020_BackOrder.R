#================ About ================
# Pasos de ejecución:
#   Pedidos Pendientes
#     1.- Extraccion de info base
#     2.- Limpia y formato de la informacion a usar
#     3.- quitamos los shipments que van a llegar
#     4.- Cruce, filtro y consolidacion de informacion a usar

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
  mutate(BANNER = case_when(
      CEDIS == "NARANJO" ~ "MV",
      CEDIS == "PINO" ~ "LUX",
      CEDIS == "CANCUN" ~ "SGI",
      TRUE ~ NA_character_  # Corrección: NA explícito para tipo character  # En caso de que haya otros valores, los mantenemos como están
    )) %>% 
  rename(SHIPMENT = STARS) %>% 
  select(ID_PEDIDO, SHIPMENT, CANTIDAD, ENTREGADO, PENDIENTE, SKU, EAN, ESTADO, PEDIDO_PEND, BANNER, FECHA, ANIO, MES, SEMANA)

#Cruza, Filtra y Agrupa Informacion de PedidosPendientes
tPed_Pendiente <- q001PPendientesClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") %>% #Cruce con Skus
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  filter(PEDIDO_PEND == "Y") %>% #Filtro pedido pendiente
  filter(ANIO == as.integer(cAnio)) %>% 
  filter(SEMANA == as.integer(cSemana)) %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))


#Lista de data frames a conservar
vGuarda <- c("tPed_Pendiente") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
