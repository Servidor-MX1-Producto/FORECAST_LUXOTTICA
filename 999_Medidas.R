#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Ejecucion ===================
#DataFrame Base
tDataFrameCons <- rbind(tInventario[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tPed_Pendiente[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tShipment[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tVenta[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tFacing[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")],
                        tSellOut[,c("BANNER", "FRAMES_SUN", "PACK", "ID_LINEA")]) %>% 
  unique() %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#Cruce de datos (Data Frame base)
tDataFrameBase <- tDataFrameCons %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|")) %>% 
  left_join(tFacing[,c("ID_BFPL", "FACING_ESTIMADO")], by = "ID_BFPL") %>% #Facing
  left_join(tSellOut[,c("ID_BFPL", "SELLOUT")], by = "ID_BFPL") %>% #Sell Out
  left_join(tPed_Pendiente[,c("ID_BFPL", "PENDIENTE")], by = "ID_BFPL") %>% #Pedidos Pendientes
  left_join(tShipment[,c("ID_BFPL", "SHIPMENT")], by = "ID_BFPL") %>% #Shipment
  left_join(tInventario[,c("ID_BFPL", "INVENTARIO")], by = "ID_BFPL") %>% #Shipment
  left_join(tVenta[,c("ID_BFPL", "VENTA")], by = "ID_BFPL") %>% 
  mutate_at(c("FACING_ESTIMADO", "SELLOUT", "PENDIENTE", "SHIPMENT", "INVENTARIO", "VENTA"),~replace(., is.na(.), 0)) %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, FACING_ESTIMADO, INVENTARIO, PENDIENTE, SHIPMENT, SELLOUT, VENTA, ID_BFPL)
  

#SellOut pendiente
#   Al SellOut estimado le quitamos la venta que llevamos al momento al mes
q000SellOutPend <- tDataFrameBase %>% 
  mutate(SELLOUT_PENDIENTE = ifelse((SELLOUT - VENTA) < 0, 0, (SELLOUT - VENTA))) #Contemplamos posible error de negativos al calcular la resta

#Inventario Ideal
#   Contemplamos el Facing estimado y el SellOut estimado del proximo mes (Inventario Ideal para iniciar el siguiente mes)

#Facing estimado del siguiente mes
q001FacingNextMes <- tFacingEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, (as.numeric(cMes) + 1) + 5) %>% 
  rename(FACING_ESTIMADO_m1 = 5) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#SellOut estimado del siguiente mes
q001SellOutNextMes <- tSellOutEst %>% 
  select(BANNER, FRAMES_SUN, PACK, ID_LINEA, (as.numeric(cMes) + 1) + 5) %>% 
  rename(SELLOUT_m1 = 5) %>% 
  mutate(ID_BFPL = paste(BANNER, FRAMES_SUN, PACK, ID_LINEA, sep = "|"))

#   Sumamos el Facing Estimado (m + 1) mas SellOut estimado (m + 1)
q001InvIdeal <- tDataFrameCons %>% 
  left_join(q001FacingNextMes[,c("ID_BFPL", "FACING_ESTIMADO_m1")], by = "ID_BFPL") %>% 
  left_join(q001SellOutNextMes[,c("ID_BFPL", "SELLOUT_m1")], by = "ID_BFPL") %>%
  mutate_at(c("SELLOUT_m1", "FACING_ESTIMADO_m1"),~replace(., is.na(.), 0)) %>%
  mutate(INV_IDEAL = SELLOUT_m1 + FACING_ESTIMADO_m1)

#Inventario Recibido
q002InvRec <- tDataFrameBase %>% 
  mutate(INV_RECIBIDO = PENDIENTE + SHIPMENT)

#Inv final de mes
q003InvFin <- tDataFrameBase %>% 
  mutate(SELLOUT_PENDIENTE = ifelse((SELLOUT - VENTA) < 0, 0, (SELLOUT - VENTA))) %>% 
  mutate(INV_RECIBIDO = PENDIENTE + SHIPMENT) %>% 
  left_join(q001InvIdeal[,c("ID_BFPL", "INV_IDEAL")], by = "ID_BFPL") %>% 
  mutate(INV_FIN = INVENTARIO + INV_RECIBIDO - SELLOUT_PENDIENTE)

q004Ncsd <- q003InvFin %>% 
  mutate(NECESIDAD = ifelse((INV_IDEAL - INV_FIN) < 0, 0, (INV_IDEAL - INV_FIN)))


