#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================
#Pedidos Pendientes
tPedidos_Pendientes <- read.csv(file.path(rUser, rSharePoint, "Compras", "Pedidos", "01_Pedidos_Pendientes", "Pedidos_Pendientes.csv"), header = TRUE, sep = ",") %>% 
  mutate(EAN = fRight(EAN,13))

#Shipments
#Lux
tShipments_Lux <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Lux", "GV_MX_ASN_LUX_Cy_Ly.csv"), header = TRUE, sep = ",")

#Sgi
tShipments_Sgi <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Solaris", "GV_MX_ASN_SOL_Cy_Ly.csv"), header = TRUE, sep = ",")

#Mv
tShipments_Mv <- read.csv(file.path(rUser, rSharePoint, "Reportes", "Compras", "02_Reportes", "Mas Vision", "GV_MX_ASN_MV_Cy_Ly.csv"), header = TRUE, sep = ",")

#================ Ejecucion ===================
#Limpieza Pedidos Pendientes
x <- tPedidos_Pendientes %>% 
  select(Stars, Estado, EAN, Fecha_Promesa, )
  