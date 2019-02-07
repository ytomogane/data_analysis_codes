data=read.delim('/Users/tomokinn/Google ドライブ/c_for_analyze08022.txt')
anal=data[17:293]
heats=as.matrix(anal)
heatsrow=data[,c(6)]
rownames(heats)=paste(heatsrow)
par(ps=3)
Heatmap(heats,column_title = "PCA for 17Yr-, normal others tumors", 
        row_title_gp = gpar(fontsize = 20),column_title_gp = gpar(fontsize = 10),
        row_names_gp = gpar(fontsize = 2),column_names_gp = gpar(fontsize = 2), 
        row_dend_width = unit(30, "mm"),column_dend_height = unit(30, "mm"))


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
write.table(scores(pcIr),file="a_0824scores.csv", sep=",")
write.table(loadings(pcIr),file="a_0824loadings.csv", sep=",")
write.table(summary(pcIr),file="a_0824summary.csv", sep=",")
