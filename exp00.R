#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection






#################### Parametros para cambiar ######################
#p_ruta_scripts = "~/tmp/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/scripts"
p_ruta_scripts = "~/Posgrado/UTN Mineria de Datos/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/git/AMDCyT"

p_ruta = "~/Posgrado/UTN Mineria de Datos/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/Dataset/datos/covid/"
#p_ruta = "~/tmp/10-Aplicaciones de Minería de Datos Ciencias y Tecnología/Dataset/datos/covid/"

p_carpeta_exp = "exp"

# Numero de Experimento
p_experimento = "00"
# path a la carpeta base
p_carpeta_base <- "datasets/"
# nombre del archivo a importar como dataset
p_archivo_dataset <- "covid_utn2022.rds"
# nombre del archivo con el 20% final a predecir
p_archivo_dataset_final20 <- "covid_utn2022_unknown.rds"
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

kBO_iter  <- 2   #cantidad de iteraciones de la Optimizacion Bayesiana

# Prefijo para la carpeta de predicciones
kprefijo       <- paste0("KA", p_experimento)

# Hiperparametros de la bayesiana
plearning_rate_lower = 0.01
plearning_rate_upper =  0.3
pfeature_fraction_lower = 0.2
pfeature_fraction_upper = 1.0
pmin_data_in_leaf_lower  = 0
pmin_data_in_leaf_upper  = 4000
pnum_leaves_lower = 10L
pnum_leaves_upper = 1024L


#------------------------------------------------------------------------------
#graba a un archivo mensajes de loggin

file_log  <- function( reg, arch="log", ext=".txt", verbose=TRUE )
{
  archivo  <- paste0(arch, ext )
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}



# Función que utilizamos para hacer el Feature Engineering
FE  <- function( ds )
{
  # Quitamos variables con muchos NA y que no influyan
  # ds = ds[,-c('embarazo','metodo_otro',
  #             'no_neumonia_cual','progr_imagerad_perc',
  #             'otra_medicacion',
  #             'altura')]
  # Agregamos campo días enfermo (Desde el útlimo diagnóstico) ! no tenemos fecha de alta(médica, defuncion)
  #ds[ , diasenfermo :=as.numeric(difftime("2022-1-21", fdiag_covid, units = "days")) ]
  
  #paso la clase a binaria que tome valores {0,1}  enteros
  
  if("resultado" %in% colnames(ds))
  {
    ds[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]
  }
  
  # Agregamos campo dias entre test y síntomas
  ds[ , diastestsintoma :=as.numeric(difftime(fdiag_covid, fini_sintomas, units = "days")) ]
  # Agregamos campo dias entre test e internación
  ds[ , diastestinternacion :=as.numeric(difftime(finternacion, fdiag_covid, units = "days")) ]
  # Agregamos campo dias entre sintomas e internación
  ds[ , diassintomainternacion :=as.numeric(difftime(finternacion, fini_sintomas, units = "days")) ]
  return( ds )
}


setwd(p_ruta)

# Creamos la carpeta exp donde estarán todos los exper
dir.create( p_carpeta_exp,  showWarnings = FALSE ) 

# Creamos la carpeta del experimento
dir.create( paste0(p_carpeta_exp, "/", kprefijo, "/" ), showWarnings = FALSE )

#Establezco el Working Directory DEL EXPERIMENTO
setwd( paste0(p_carpeta_exp, "/", kprefijo, "/" ) )   



# Llamamos a la Bayesiana
source(paste0(p_ruta_scripts, "/01_lightGBM_BO_COVID.R"))

# Llamamos al testing
source(paste0(p_ruta_scripts, "/02_testing_COVID.R"))

# Graficamos y ROC
source(paste0(p_ruta_scripts, "/03_curva_roc.R"))


file_log(reg="Fin del Script!")
