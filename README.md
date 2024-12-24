# FORECAST_LUXOTTICA

## **INTRODUCCIÓN:**
### **Descripción General**
Proyecto utilizado para generar un calculo futuro de compras.

### **Objetivos**
- Generar calculo a futuro de que producto y que cantidad comprar.

### **Alcance**
Este proyecto se centra exclusivamente en el calculo a futuro de la compra.

## **INSTALACIÓN:**
### **Requisitos del Sistema**
- **Hardware:** Procesador de 2 GHz, 4 GB de RAM, 500 MB de espacio en disco.
- **Sistema Operativo:** Windows 10, macOS Catalina, Linux Ubuntu 18.04+.
- **Dependencias:** R 4.0+, Power_BI Desktop.

### **Instrucciones de Instalación:**
1. Instalar el CRAN de R
2. Instalar RStudio
3. Instalar Git
4. Clonar repositorio **(git remote origin https://github.com/...)**

***

## **USO DEL SOFTWARE:**
### **Guia del Usuario**
- Ejecución
    - Abrir el proyecto con RStudio
    - Abrir script **000_Hechos.R**
    - Ejecutar, ya que el script contiene el flujo nesesario del proyecto
    
- Desarrollo
    - Abrir el proyecto con RStudio
    - Abrir script **000_Hechos.R**
    - Definir la variable Modo en 1, que define la forma de desarrollo.
    
    
[!TIP]
Asegurar que las carpetas se encuentren en la ruta correcta, ya que se utilizan en el proceso del poyecto.
***

## **ARQUITECTURA DEL SOFTWARE:**
### **Flujo del Proyecto**

```mermaid
graph TD
    A([INICIO]) 
    B[CÁLCULO INVENARIO]
    C[CÁLCULO BACKORDER]
    D([FIN])
```

## **SCRIPTS:**
Los nombres de los Scripts comienzan por un número asigando a la funcion que realiza el script.


| Número | Función |
| :----: | :-----: | 
| 000 | Actualización de catálogos y paths|
| 010 | Inventario |
| 020 | Backorder |


Dentro del código del proyecto, en la parte final de este, existe un pequeño query que funciona para limpiar el Environment del proyecto, dentro de este query esta una lista para agregar datos que se pueden reutilizar en el futuro de la ejecución

``` 
  #Lista de data frames a conservar
  vGuarda <- c(ls()) #Agregar datos que se guardan en el environment
  vMantener <- c(vMantener, vGuarda)
  vBorrar <- setdiff(ls(), vMantener)
  
  rm(list = vBorrar)
  rm(vBorrar)
```

### **000_Hechos.R**
#### **Descripción**
Script que lleva el flujo del proyecto, definiendo los dataframes, variables, constantes, funciones, conjunto de datos, etc. necesarios para los calculos que se generan mas adelante.

El proyecto tiene modo de Desarrollo o modo Despligue, modo desarrollador es para agregar cambios en el código y el modo despligue se ejecuta en el servidor:

| Modo | Descripción | ID | 
| :----: | :-----: | :----: |
| Desarrollo | Trabaja con catalogos locales que se encuentren en la memoria del equipo, por lo cual **es necesario la ruta de carpetas este definida correctamente, tal cual se encuentra el Sharepoint.** | 1 |
| Despliegue | Trabaja con los catalogos concentrados en SharePoint. | 0 |

#### **Diagrama de Flujo**

```mermaid
graph TD
    A([INICIO]) --> B[DEFINE_LIBRERIAS]
    B --> C[DEFINE FUNCIONES]
    C --> D[DEFINE PATHS]
    D --> E[DEFINE CONSTANTES]
    E --> F[EJECUTA 010_Inventarios.R]
    F --> G[EJECUTA_020_BackOrder.R]
    G --> H([FIN])
```

#### **Reglas**
-

### **010_Inventarios.R**
#### **Descripción**
Este script nos permite delimitar y estructurar el inventario con el que vamos a trabajar en los calculos.

#### **Diagrama de Flujo**
```mermaid
flowchart TD
A([INICIO])
B[SELECCIONA CATALOGOS DE INVENTARIOS DE CADA BANNER]
C[SELECCIONA CATALOGO DE SUCURSALES Y DE ARTICULOS]
D[CRUCE DE INFORMACION]
E[FILTRO DE SUCURSALES A CONSIDERAR]
F[AGRUPACION POR BANNER, TIPO, LINEA y PACK]
G([FIN])


A --> B
B --> C
C --> D
D --> E
E --> F
F --> G
```

#### **Reglas**
-

### **020_FMT_Borra_Samba.R**
#### **Descripción**
Este script está diseñado para realizar tareas de mantenimiento en la carpeta Samba, permitiendo la eliminación progresiva de controles generados en días anteriores.

#### **Diagrama de Flujo**
```mermaid
flowchart TD
A([INICIO])
B[DEFINE CONSTANTES Y/O PATHS EXTRAS]
C[LEE ARCHIVOS EN CARPETA Samba]
D[OBTENEMOS FECHA DE CREACION DE CADA ARCHIVO]
E{ELIMINAR?}
EY[ELIMINA ARCHIVOS]
F([FIN])


A --> B
B --> C
C --> D
D --> E
E --> |SI| EY
E --> |NO| F
EY --> F
```

#### **Reglas**
-


## **REPORTES PBI:**
No hay reportes de Power Bi que consuman reportes de este proyecto

## **HORAS DE EJECUCIÓN:**

#### **Horario**

| Día | Hora | 
| :----: | :-----: |
| Lunes | 6:20 am |  
| Martes | 6:20 am |  
| Miércoles | 6:20 am |  
| Jueves | 6:20 am |  
| Viernes | 6:20 am | 
| Sábado | 6:20 am |  
| Domingo | 6:20 am | 

#### **Archivo de Ejecución**
DIST_FMT_GLOBAL.bat



