
setwd(paste0(p_ruta,'exp/lightGBM/'))

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




