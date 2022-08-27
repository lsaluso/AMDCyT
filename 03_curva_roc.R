# Librerias para curvas ROC
library(tidyverse)
library(pROC)


file_log(reg="Comenzamos ROC.")

tb_entrega %>% 
  arrange(desc(pred)) %>% 
  mutate(indice=seq(1:nrow(tb_entrega))) %>%
  ggplot(aes(indice,pred)) +
  geom_line()
ggsave(paste0(p_experimento, "_probabilidad.png"))

tb_entrega %>% 
  arrange(desc(pred)) %>%
  ggplot(aes(pred)) +
  geom_density()
ggsave(paste0(p_experimento, "_densidad.png"))

tb_entrega %>% 
  arrange(desc(pred)) %>%
  ggplot(aes(pred,fill=factor(clase01))) +
  geom_density(alpha=0.5)
ggsave(paste0(p_experimento, "_densidad_0_1.png"))


# Hacemos la curva ROC
roc <- roc(tb_entrega$clase01,tb_entrega$pred,grid=T)
png(paste0(p_experimento, "_ROC.png"), width = 800, height = 800)
plot(roc,col="red", xlim=c(1,0))
dev.off()

# Guardamos en un archivo el área bajo la curva
fileConn<-file(paste0(p_experimento, "_ROC.txt"))
writeLines(toString(roc[["auc"]]), fileConn)
close(fileConn)

# Probabilidad de Corte seleccionada
probCorte = 0.119

tb_entrega[,clasif:=ifelse(pred>probCorte,1,0)]
matrizConfusion=table(realidad=tb_entrega$clase01,prediccion=tb_entrega$clasif)
TP = matrizConfusion[2,2]
FP = matrizConfusion[1,2]
FN = matrizConfusion[2,1]
TotalPositivos = TP+FP
file_log(reg=paste0("True Positive:", TP))
file_log(reg=paste0("False Positive:", FP))
file_log(reg=paste0("False Negativos:", FN))
file_log(reg=paste0("Positivos totales:", TotalPositivos))


tb_entrega_final20[,clasif:=ifelse(pred>probCorte,1,0)]

prediccionFinal = tb_entrega_final20[,c('codigo_paciente','clasif')]
write.table(prediccionFinal, 'prediccion_final_20.csv', sep = "\t", 
            row.names = FALSE)

sumPositivos = sum(prediccionFinal[,c('clasif')])

file_log(reg=paste0("Totales 1s en Predicción Final:", sumPositivos))
