data=read.delim('/Users/tomokinn/Google ドライブ/a_PS_for_analyze08022.txt')
anal=data[17:292]
pcIr <- pca(anal, method="nipals", nPcs=30, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
age <- data[,5]
label <- as.factor(data[,6])
percent <- summary(pcIr)[2,2] * 100
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)
label
write.table(scores(pcIr),file="a_PS_0824scores.csv", sep=",")
write.table(loadings(pcIr),file="a_PS_0824loadings.csv", sep=",")
write.table(summary(pcIr),file="a_PS_0824summary.csv", sep=",")
