data=read.delim('/Users/tomokinn/Google ドライブ/TEXTall_for_analyze08017.txt')
anal=data[5:288]
pcIr <- pca(anal, method="nipals", nPcs=30, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
label <- as.factor(data[,3])
percent <- summary(pcIr)[2,2] * 100
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)
