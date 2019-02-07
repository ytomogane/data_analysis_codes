data=read.csv('/Users/tomokinn/Google ドライブ/AComStaAbu_afterMICE_Text09072017.csv')
anal=data[18:425]
pcIr <- pca(anal, method="nipals", nPcs=150, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
age <- data[,5]
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