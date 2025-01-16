#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Pedidos Pendientes
tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
  rename_all(toupper) %>% 
  mutate(EAN = fRight(EAN, 13))

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
  rename_all(toupper) %>% 
  mutate(FECHA = as.Date(FECHA, format("%Y-%m-%d"))) %>% 
  mutate(SEMANA = as.numeric(strftime(FECHA, format("%V")))) %>% 
  mutate(MES = month(FECHA)) %>% 
  mutate(ANIO = year(FECHA)) %>% 
  mutate(EAN = fRight(EAN, 13)) %>% 
  select(SHIPMENT, EAN, SKU, CANTIDAD, EMPRESA, FECHA, SEMANA, MES, ANIO)

#================ Ejecucion ===================
#Limpieza Pedidos Pendientes
q001PPendientesClean <- tPedidos_Pendientes %>% 
  rename(SHIPMENT = STARS) %>% 
  select(SHIPMENT, ESTADO, EAN, FECHA_PROMESA, CANTIDAD)

#filtro de semana en Shipments
q002WeekShipment <- tShipments %>% 
  rename(CANTVALIDA = CANTIDAD) %>% #Renombre de columna que nos ayuda a validar en futuros cruces
  filter(SEMANA == cSemana)

#Cruce de datos para filtrar 
q003ShipPend <- q001PPendientesClean %>% 
  left_join(q002WeekShipment[,c("SHIPMENT", "CANTVALIDA", "EMPRESA")], by = "SHIPMENT") %>% 
  filter(!(is.na(CANTVALIDA))) %>% #
  filter(!(ESTADO == "Cancelado")) %>% 
  select(SHIPMENT, ESTADO, EAN, FECHA_PROMESA, CANTIDAD, EMPRESA)

#Generar 3 dataframes(Compras, Pendiente, Shipment)
#Dataframe base
q004FrameBase <- q003ShipPend %>% 
  left_join(tArt_Cat[,c("ID_TIPO", "ID_LINEA", "EAN", "PACK", "ID_GENERICO")], by = "EAN") 

#Compra
q004Compra <- q004FrameBase %>% 
  group_by(EMPRESA, ID_TIPO, ID_LINEA, PACK) %>% 
  summarise(CANTIDAD = sum(CANTIDAD)) #%>% 
  #filter(ESTADO == "") %>% 
  #filter(FECHA_PROMESA == "")

#Pendiente
q004Pendiente <- q004FrameBase %>% 
  group_by(EMPRESA, ID_TIPO, ID_LINEA, PACK) %>% 
  summarise(CANTIDAD = sum(CANTIDAD)) #%>% 
  #filter(ESTADO == "") %>% 
  #filter(FECHA_PROMESA == "")

#Shipment
q004Shipment <- q002WeekShipment %>% 
  left_join(tArt_Cat[,c("ID_TIPO", "ID_LINEA", "EAN", "PACK")], by = "EAN") %>% 
  rename(CANTIDAD = CANTVALIDA) %>% 
  group_by(EMPRESA, ID_TIPO, ID_LINEA, PACK) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))
  
#Lista de data frames a conservar
vGuarda <- c() #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)



