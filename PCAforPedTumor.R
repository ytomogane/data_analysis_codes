#PCAforPedTumor
data=read.csv('/Users/tomokinn/Google ドライブ/for_analyzePedTumorMRI10162017/forAnalyze_Ped2000_tumor_data_05232017_combined_M2corrected_withTumorROI_7172017_10132017withNA.csv')
anal=data[,3:17]
pcIr <- pca(anal, method="nipals", nPcs=15, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
age <- data[,5]
label <- as.factor(data[,2])
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)
label
write.table(scores(pcIr),file="Acom_for_analyze08025scores.csv", sep=",")
write.table(loadings(pcIr),file="Acom_for_analyze08025loadings.csv", sep=",")
write.table(summary(pcIr),file="Acom_for_analyze08025summary.csv", sep=",")




read.c