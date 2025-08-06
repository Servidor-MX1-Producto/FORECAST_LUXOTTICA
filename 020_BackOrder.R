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

#Lead time
tLead_Time <- read.csv(file.path(rTablas, "LEAD_TIME.csv"), header = TRUE, sep = ",")

#================ Ejecucion ===================
#Define Lead Time de todas las lineas por proveedor
q000LTProv <- tLead_Time %>% 
  filter(ID_LINEA == "*") 

#Define Lead Time de lineas en especifico por proveedor
q000LTLinea <- tLead_Time %>% 
  filter(ID_LINEA != "*") %>% 
  mutate(ID_LPT = paste(ID_LINEA, PACK, TIPO, sep = "|"))

#Define fechas promesa en caso de ser vacias o no tener datos
q000PedidosFecha <- tPedidos_Pendientes %>% 
  mutate(ID_LPT = paste(ID_LINEA, PACK, ID_TIPO, sep = "|")) %>% 
  left_join(q000LTProv[,c("ID_PROVEEDOR", "LEAD_TIME_W")], by = "ID_PROVEEDOR") %>% #Left join para saber las lineas que se contemplan por proveedor
  mutate(LEAD_TIME_W = ifelse(ID_LPT %in% q000LTLinea$ID_LPT, q000LTLinea$LEAD_TIME_W, LEAD_TIME_W)) %>% #Condicional para saber Lead Time de lineas y proveedor en especifico
  mutate_at(c("LEAD_TIME_W"), ~replace(., is.na(.), 0)) 

#Limpieza Pedidos Pendientes
q001PPendientesClean <- q000PedidosFecha %>% 
  mutate(EAN = fRight(EAN, 13)) %>%  #Agregar validador de estados
  mutate(FECHA_PROMESA = as.Date(FECHA_PROMESA, format("%d/%m/%Y"))) %>% 
  mutate(FECHA_CAP = as.Date(FECHA_CAP, format("%d/%m/%Y"))) %>%
  mutate(FECHA = if_else(is.na(FECHA_PROMESA), FECHA_CAP + weeks(LEAD_TIME_W), FECHA_PROMESA)) %>% #En caso de no tener fecha promesa le agregaremos a la fecha de captura las n semanas de LEAD TIME ya definidas
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
  rename(TIPO = ID_TIPO) %>% 
  select(ID_PEDIDO, SHIPMENT, CANTIDAD, ENTREGADO, PENDIENTE, SKU, EAN, ID_LINEA, PACK, TIPO, ID_GENERICO, ID_PROVEEDOR, ESTADO, PEDIDO_PEND, ID_EMPRESA, FECHA, ANIO, MES, SEMANA)

#Cruza y filtra Informacion 
q001InfoFormat <- q001PPendientesClean %>% 
  mutate(PACK = ifelse(ID_EMPRESA != c(11) | is.na(PACK) | nchar(PACK) == 0, "-", PACK)) %>% #Solo se considera Pack para Mas Vision
  filter(ID_GENERICO == "A") %>% #Filtro para conservar solo Armazones
  filter(ID_PROVEEDOR == "LUM") %>% #Filtra Tipo de Marca
  filter(PEDIDO_PEND == "Y") %>% #Filtro pedido pendiente
  select(ID_EMPRESA, ANIO, SEMANA, ID_LINEA, PACK, TIPO, PENDIENTE)

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
vGuarda <- c("tPed_Pendiente", "tLead_Time", "q000LTProv", "q000LTLinea") #Agregar datos que se guardan en el environment
vMantener <- c(vMantener, vGuarda)
vBorrar <- setdiff(ls(), vMantener)

rm(list = vBorrar)
rm(vBorrar)
