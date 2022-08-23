#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


# Librerías para la Bayesiana
require("data.table")
require("rlist")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

# Librerías para Testing
require("data.table")
require("lightgbm")
library(caret)

# Librerias para curvas ROC
library(tidyverse)
library(pROC)


#################### Parametros para cambiar ######################
#p_ruta_scripts = "~/tmp/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/scripts"
p_ruta_scripts = "~/Posgrado/UTN Mineria de Datos/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/scripts"

p_ruta = "~/Posgrado/UTN Mineria de Datos/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/Dataset/datos/covid/"
#p_ruta = "~/tmp/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/Dataset/datos/covid/"
setwd(p_ruta)

# Numero de Experimento
p_experimento = "08"
# path a la carpeta base
p_carpeta_base <- "./datasets"
# nombre del archivo a importar como dataset
p_archivo_dataset <- "covid_utn2022.rds"
# nombre de la columna que usare como clase
columna_clase <- "resultado"
# prefijo para nombrar los archivos de salida (el .RDATA y el .txt)
p_etiqueta_archivos_salida <- paste0("COVID", p_experimento)
# parametro a minimizar (si desea maximizar debe cambiar a FALSE la linea del obj.fun)
p_objective <- "binary" # esto si es un problema con clase numérica. Si la clase es binaria, cambiar por "binary"
p_parametro_optimizar <- "auc" # cambiar por lo que estemos estimando. Si es un problema de clasificacion, cambiar por "auc"
# Consultar lista en https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
# IMPORTANTE: no usar los alias de esa lista, sino la denominación principal

p_minimize <- FALSE # Si se quiere maximizar un parámetro cambiar por FALSE, por ejemplo en un problema de clasificacion
ksemilla_azar  <- 455531  #Aqui poner la propia semilla

kBO_iter  <- 5   #cantidad de iteraciones de la Optimizacion Bayesiana

# Prefijo para la carpeta de predicciones
kprefijo       <- paste0("KA", p_experimento)


# Función que utilizamos para hacer el Feature Engineering
FE  <- function( ds )
{
  # Quitamos variables con muchos NA y que no influyan
  ds = ds[,-c('embarazo','metodo_otro',
                        'no_neumonia_cual','progr_imagerad_perc',
                        'otra_medicacion',
                        'altura')]
  # Agregamos campo días enfermo (Desde el útlimo diagnóstico) ! no tenemos fecha de alta(médica, defuncion)
  #ds[ , diasenfermo :=as.numeric(difftime("2022-1-21", fdiag_covid, units = "days")) ]
  # Agregamos campo dias entre test y síntomas
  ds[ , diastestsintoma :=as.numeric(difftime(fdiag_covid, fini_sintomas, units = "days")) ]
  # Agregamos campo dias entre test e internación
  ds[ , diastestinternacion :=as.numeric(difftime(finternacion, fdiag_covid, units = "days")) ]
  # Agregamos campo dias entre sintomas e internación
  ds[ , diassintomainternacion :=as.numeric(difftime(finternacion, fini_sintomas, units = "days")) ]
  return( ds )
}


# Llamamos a la Bayesiana
setwd(p_ruta)
source(paste0(p_ruta_scripts, "/01_lightGBM_BO_COVID.R"))

# Llamamos al testing
setwd(p_ruta)
source(paste0(p_ruta_scripts, "/02_testing_COVID.R"))

# Graficamos y ROC
setwd(p_ruta)
source(paste0(p_ruta_scripts, "/03_curva_roc.R"))

