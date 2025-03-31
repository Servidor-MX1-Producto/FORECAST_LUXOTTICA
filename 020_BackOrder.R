#================ About ================
# Pasos de ejecución:
#   Shipment
#     1.- Extraccion de info base por cada banner
#     2.- Consolida info de cada banner
#     3.- Limpia y formato de la informacion a usar
#     4.- Cruce, filtro y consolidacion de informacion a usar
#   Pedidos Pendientes
#     1.- Extraccion de info base
#     2.- Limpia y formato de la informacion a usar
#     3.- quitamos los shipments que van a llegar
#     4.- Cruce, filtro y consolidacion de informacion a usar

#================ Importaciones ===================
#Pedidos Pendientes
tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) 

#Shipments
#Lux
tShipments_Lux <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Lux", "GV_MX_ASN_LUX_Cy_Ly.csv"), header = TRUE, sep = ",")

#Sgi
tShipments_Sgi <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Solaris", "GV_MX_ASN_SOL_Cy_Ly.csv"), header = TRUE, sep = ",")

#Mv
tShipments_Mv <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Mas Vision", "GV_MX_ASN_MV_Cy_Ly.csv"), header = TRUE, sep = ",")

#================ Ejecucion Shipments ===================

#Shipments consolidados
tShipments <- rbind(
  tShipments_Lux[,c("Shipment", "Fecha_Samba", "EAN", "SKU", "Cantidad", "Empresa")],
  tShipments_Mv[,c("Shipment", "Fecha_Samba", "EAN", "SKU", "Cantidad", "Empresa")],
  tShipments_Sgi[,c("Shipment", "Fecha_Samba", "EAN", "SKU", "Cantidad", "Empresa")]
  ) %>% 
  rename_all(toupper) 

#Limpieza de Shipments
q001ShipmentsClean <- tShipments %>% 
  mutate(EAN = fRight(EAN, 13)) %>% 
  mutate(FECHA = as.Date(FECHA_SAMBA, format("%Y-%m-%d"))) %>% #Formato de fechas
  group_by(SHIPMENT, FECHA, EAN, SKU, EMPRESA) %>% #Se agrupa para sumar cantidades
  summarise(CANTIDAD = sum(CANTIDAD)) %>% 
  mutate(SEMANA = as.numeric(strftime(FECHA, format("%V")))) %>% #Define semana
  mutate(MES = month(FECHA)) %>% #Define mes
  mutate(ANIO = year(FECHA)) %>% #Define anio  
  mutate(BANNER = case_when(
    EMPRESA == 7 ~ "SGI",
    EMPRESA == 2 ~ "LUX",
    EMPRESA == 11 ~ "MV",
    TRUE ~ NA_character_  # Corrección: NA explícito para tipo character  # En caso de que haya otros valores, los mantenemos como están
  )) %>% 
  select(SHIPMENT, EAN, SKU, CANTIDAD, BANNER, FECHA, ANIO, MES, SEMANA) 

#Cruza, Filtra y Agrupa Informacion de Shipments (info para quitar de Pedidos Pendientes)

#Obtenemos la fecha de 14 dias atras (2 semanas)
cSemanaAtras <- today() - days(14)

#Info limpia
q002ShipsFilters <- q001ShipmentsClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") %>%
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  #filter(MES == as.integer(cMes)) %>%
  #Filtramos anio y semana respecto a la fecha asignada de cSemanasAtras
  filter(ANIO == year(cSemanaAtras)) %>%
  filter(SEMANA == as.numeric(strftime(cSemanaAtras, format("%V")))) %>% #Traemos los shipments de la semana anterior a la de la fecha de ejecucion
  group_by(SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% #Agrupacion por Shipments
  summarise(CANTIDAD = sum(CANTIDAD)) %>% 
  mutate(ID_SBFPL = paste(SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Organizamos y damos formato a los Shipments
tShipment <- q002ShipsFilters %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% #Agrupacion por Banner
  summarise(SHIPMENT = sum(CANTIDAD)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#================ Ejecucion Pedidos Pendientes ===================
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
q002PPendFilters <- q001PPendientesClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") %>% #Cruce con Skus
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  filter(PEDIDO_PEND == "Y") %>% #Filtro pedido pendiente
  filter(ANIO == as.integer(cAnio)) %>% 
  filter(MES == as.integer(cMes)) %>% 
  # filter(SEMANA == as.integer(cSemana) - 1) #%>% 
  group_by(ID_PEDIDO, SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% #Se agrupa con ID_PEDIDO y Shipment para depues quitar lo que traemos en Shipment
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_SBFPL = paste(SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Quitanmos los shipments de Pedidos Pendientes
tPed_Pendiente <- q002PPendFilters %>% 
  left_join(q002ShipsFilters[,c("ID_SBFPL", "CANTIDAD")], by = "ID_SBFPL") %>% #Cruzamos con el Shipment para quitarlo
  mutate_at(c("CANTIDAD"),~replace(., is.na(.), 0)) %>%
  mutate(PENDIENTE = PENDIENTE - CANTIDAD) %>% #Quitamos cantidades del shipment
  filter(PENDIENTE > 0) %>%  #Filtramos lo que si tengamos pendiente
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))


#Lista de data frames a conservar
vGuarda <- c("tPed_Pendiente", "tShipment") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
