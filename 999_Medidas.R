#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Ejecucion ===================
#DataFrame Base
tDataframeBase <- rbind(tInventario[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tPed_Pendiente[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tShipment[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tVenta[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tFacing[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tSellOut[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")]) %>% 
  unique()

#Cruce de datos
tX <- tDataframeBase %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|")) %>% 
  left_join(tFacing[,c("ID_BFPL", "FACING")], by = "ID_BFPL") %>% #Facing
  left_join(tSellOut[,c("ID_BFPL", "SELLOUT")], by = "ID_BFPL") %>% #Sell Out
  left_join(tPed_Pendiente[,c("ID_BFPL", "PENDIENTE")], by = "ID_BFPL") %>% #Pedidos Pendientes
  left_join(tShipment[,c("ID_BFPL", "SHIPMENT")], by = "ID_BFPL") %>% #Shipment
  left_join(tInventario[,c("ID_BFPL", "INVENTARIO")], by = "ID_BFPL") %>% #Shipment
  left_join(tVenta[,c("ID_BFPL", "VENTA")], by = "ID_BFPL") %>% 
  mutate_at(c("FACING", "SELLOUT", "PENDIENTE", "SHIPMENT", "INVENTARIO", "VENTA"),~replace(., is.na(.), 0)) 
  

#Medida: Otro Inventario
tX1 <- tX %>% 
  mutate(INVENTARIOX2 = PENDIENTE + SHIPMENT + EXISTENCIA)

tX2 <- tX1 %>% 
  mutate(FACING_INVSEG = FACING + VENTA) %>% 
  mutate(INV_FINAL = INVENTARIOX2 + SHIPMENT - VENTA)
