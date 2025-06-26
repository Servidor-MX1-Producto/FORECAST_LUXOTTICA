#================ About ================
# Pasos de ejecución:
#   1.- 
#   2.- 
#   3.- 

#================ Importaciones ===================

#================ Ejecucion ===================
#Definire Lead Time real y define ID Semana
q001IdSemana <- tNecesidad %>% 
  mutate(DATE_LT = ISOweek2date(sprintf("%d-W%02d-1", as.numeric(cAnio), as.numeric(cSemana))) + weeks(LEAD_TIME_W)) %>% 
  mutate(SEMANA_LT = as.numeric(strftime(DATE_LT, format("%V")))) %>% 
  mutate(cDelta01 = ifelse(SEMANA >= SEMANA_LT, 1, 0)) %>% #Etiquetamos semanas en las que nos puede llegar producto, tomando en cuenta el Lead Time
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% #Agrupamos para generar ID_SEMANA
  mutate(ID_SEMANA = cumsum(cDelta01))

#Calculo Requerimiento
q001CalcReq <- q001IdSemana %>% 
  filter(cDelta01 > 0) %>% #Filtro solo las semanas en las que nos puede llegar producto, contemplando el Lead Time
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% 
  mutate(REQUERIMIENTO = NECESIDAD - lag(NECESIDAD, default = 0)) %>% #Utilizamos la funcion lag para hacer calculo entre rows anteriores, restando la necesidad conforma va avanzando las semanas
  mutate(ID_ELPTS = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, ID_SEMANA, sep = "|")) #Generamos ID con la semana para cruzar el requerimiento semanalmente, dodne la semana 1 es la semana despues del Lead time

#Estructura al reporte de requerimiento
q001Requerimiento <- q001IdSemana %>% #Tomamos dataframe base para realizar los cruces
  select(ID_EMPRESA, ID_LINEA, PACK, TIPO, INVENTARIO, PENDIENTE, FORECAST, STOCK_SEGURIDAD, FACING, INV_F, INV_I, NECESIDAD, SEMANA, ANIO, ID_PROVEEDOR) %>% 
  mutate(cDelta02 = 1) %>% 
  group_by(ID_EMPRESA, ID_LINEA, PACK, TIPO) %>% #Agrupamos para generar ID_SEMANA
  mutate(ID_SEMANA = cumsum(cDelta02)) %>% 
  mutate(ID_ELPTS = paste(ID_EMPRESA, ID_LINEA, PACK, TIPO, ID_SEMANA, sep = "|")) %>% #ID con Semana 
  left_join(q001CalcReq[,c("ID_ELPTS", "REQUERIMIENTO")], by = "ID_ELPTS") %>% #Cruce con el requerimiento previamente calculado
  mutate_at(c("REQUERIMIENTO"), ~replace(., is.na(.), 0)) %>% 
  mutate(REQUERIMIENTO = ifelse(REQUERIMIENTO < 0, 0, REQUERIMIENTO)) %>% 
  data.frame() %>% 
  arrange(ID_EMPRESA, ID_LINEA, PACK, TIPO, ANIO, SEMANA) %>% 
  select(ID_EMPRESA, ID_LINEA, PACK, TIPO, ID_PROVEEDOR, SEMANA, ANIO, INVENTARIO, PENDIENTE, FORECAST, STOCK_SEGURIDAD, FACING, INV_F, INV_I, NECESIDAD, REQUERIMIENTO)

#Escribe reporte de requerimeinto
write.csv(q001Requerimiento, file.path(rReportes, "REQUERIMIENTO.csv"), row.names = FALSE)

#Lista de data frames a conservar
vGuarda <- c() #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
