#PCAforPedTumor
data <- read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05022019 Pediatric tumor/forR ped tumor data summary 05142019.xlsx",
                  sheet=1)
#data=read.xlsx('C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/forR ped tumor data summary 03152019.xlsx')
#anal=data[,3:17]
anal=data[1:34,8:23]
pcIr <- pca(anal)
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
age <- data[,3]
label <- as.factor(data[,7])
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)
label
write.table(scores(pcIr),file="Acom_for_analyze08025scores.csv", sep=",")
write.table(loadings(pcIr),file="Acom_for_analyze08025loadings.csv", sep=",")
write.table(summary(pcIr),file="Acom_for_analyze08025summary.csv", sep=",")




read.c