#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Pedidos Pendientes
tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(EAN = fRight(EAN, 13)) %>%  #Agregar validador de estados
  mutate(FECHA_CAP = as.Date(FECHA_CAP, format("%d/%m/%Y"))) %>% 
  mutate(FECHA_PROMESA = as.Date(FECHA_PROMESA, format("%d/%m/%Y")))

#Shipments
#Lux
tShipments_Lux <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Lux", "GV_MX_ASN_LUX_Cy_Ly.csv"), header = TRUE, sep = ",")

#Sgi
tShipments_Sgi <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Solaris", "GV_MX_ASN_SOL_Cy_Ly.csv"), header = TRUE, sep = ",")

#Mv
tShipments_Mv <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Mas Vision", "GV_MX_ASN_MV_Cy_Ly.csv"), header = TRUE, sep = ",")

#Shipments consolidados
tShipments <- rbind(
  tShipments_Lux[,c("Shipment", "Fecha", "EAN", "SKU", "Cantidad", "Empresa")],
  tShipments_Mv[,c("Shipment", "Fecha", "EAN", "SKU", "Cantidad", "Empresa")],
  tShipments_Sgi[,c("Shipment", "Fecha", "EAN", "SKU", "Cantidad", "Empresa")]
  ) %>% 
  rename_all(toupper) 

#================ Ejecucion ===================
#Limpieza Pedidos Pendientes
q001PPendientesClean <- tPedidos_Pendientes %>% 
  mutate(BANNER = case_when(
      CEDIS == "NARANJO" ~ "MV",
      CEDIS == "PINO" ~ "LUX",
      CEDIS == "CANCUN" ~ "SGI",
      TRUE ~ NA_character_  # Corrección: NA explícito para tipo character  # En caso de que haya otros valores, los mantenemos como están
    )) %>% 
  rename(SHIPMENT = STARS) %>% 
  select(ID_PEDIDO, SHIPMENT, FECHA_CAP, ID_ESTADO, FECHA_PROMESA, CANTIDAD, ENTREGADO, PENDIENTE, SKU, EAN, ESTADO, PEDIDO_PEND, BANNER)

#Limpieza de Shipments
q001ShipmentsClean <- tShipments %>% 
  mutate(EAN = fRight(EAN, 13)) %>% 
  mutate(FECHA = as.Date(FECHA, format("%Y-%m-%d"))) %>% #Formato de fechas
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
  select(SHIPMENT, EAN, SKU, CANTIDAD, BANNER, FECHA, SEMANA, MES, ANIO) 

#Cruza, Filtra y Agrupa Informacion de PedidosPendientes
q002PPendFilters <- q001PPendientesClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") %>% #Cruce con Skus
  filter(PEDIDO_PEND == "Y") %>% #Filtro pedido pendiente
  #filter(ESTADO == "Aprobado") %>% #Filtro que posiblemente podamos agregar
  #filter() %>% #Aqui ver si se contempla filtrar alguna fecha
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  group_by(ID_PEDIDO, SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% #Se agrupa con ID_PEDIDO y Shipment para depues quitar lo que traemos en Shipment
  summarise(PENDIENTE = sum(PENDIENTE)) %>% 
  mutate(ID_SBFPL = paste(SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Cruza, Filtra y Agrupa Informacion de Shipments
q002ShipsFilters <- q001ShipmentsClean %>% 
  left_join(tArt_Cat[,c("SKU", "ID_LINEA", "PACK", "FRAMES_SUN", "EB_EL_3P")], by = "SKU") %>%
  filter(EB_EL_3P == "EL") %>% #Filtra Tipo de Marca
  filter(ANIO == as.integer(cAnio)) %>% 
  filter(MES == as.integer(cMes)) %>% 
  filter(SEMANA == as.integer(cSemana) - 1) %>% 
  group_by(SHIPMENT, BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(CANTIDAD = sum(CANTIDAD)) %>% 
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

#Organizamos y damos formato a los Shipments
tShipment <- q002ShipsFilters %>% 
  group_by(BANNER, FRAMES_SUN, PACK, ID_LINEA) %>% 
  summarise(SHIPMENT = sum(CANTIDAD)) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))
  

#Lista de data frames a conservar
vGuarda <- c("tPed_Pendiente", "tShipment") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
