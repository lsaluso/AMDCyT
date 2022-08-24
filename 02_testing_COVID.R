


#hiperparametros de LightGBM
# si es de IT y le gusta automatizar todo, no proteste, ya llegara con MLOps

# Leemos la salida de la Bayesiana
BO = read.csv(paste0(p_ruta, 'exp/lightGBM/', p_etiqueta_archivos_salida, '.txt'), sep="\t")
BO = BO[order(-BO$parametro),]

kmax_bin           <-   BO[1,"max_bin"]
klearning_rate     <-   BO[1,"learning_rate"]
knum_iterations    <-   BO[1,"num_iterations"]
knum_leaves        <-   BO[1,"num_leaves"]
kmin_data_in_leaf  <-   BO[1, "min_data_in_leaf"]
kfeature_fraction  <-   BO[1, "feature_fraction"]

rm(list=c('BO'))


kexperimento   <- paste0( kprefijo, "" )


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#cargo el dataset donde voy a entrenar
setwd(p_carpeta_base)
dsLearn <- readRDS(p_archivo_dataset)
dsLearn <- as.data.table(dsLearn)
#paso la clase a binaria que tome valores {0,1}  enteros
dsLearn[ , clase01 := ifelse( resultado=="muerte", 1L, 0L) ]

# Hacemos el FE
dsLearn = FE(dsLearn)

set.seed(ksemilla_azar)
trainIndex <- createDataPartition(dsLearn$clase01, p = 0.8,
                                  list = F,times = 1)

dtrain <- dsLearn[trainIndex,]
dtest <- dsLearn[-trainIndex,]



setwd(p_ruta)  #Establezco el Working Directory
setwd( "./exp/lightGBM" )

columna_clase_bin <- "clase01"



#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
# dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./", kexperimento, "/" ), showWarnings = FALSE )
setwd( paste0("./", kexperimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(dsLearn[,!c(columna_clase_bin,"resultado"),with=FALSE]),
                        label= dsLearn[[columna_clase_bin]] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   metric= "auc",
                                   max_bin=            kmax_bin,
                                   learning_rate=      klearning_rate,
                                   num_iterations=     knum_iterations,
                                   num_leaves=         knum_leaves,
                                   min_data_in_leaf=   kmin_data_in_leaf,
                                   feature_fraction=   kfeature_fraction,
                                   seed=               ksemilla_azar
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo_explog.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------




#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dtest[,!c(columna_clase_bin,"resultado"),with=FALSE])
                        ,type="prob")

#genero la tabla de entrega
tb_entrega  <-  dtest[ , list( codigo_paciente,clase01 ) ]
tb_entrega[  , pred := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion_explog.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, pred )

  
