data=read.delim('/Users/tomokinn/Google ドライブ/Acomabsolute_for_analyze08025scores.txt')
anal=data[18:34]
heats=as.matrix(anal)
heatsrow=data[,c(7)]
rownames(heats)=paste(heatsrow)
par(ps=3)
my.col1 <- colorRampPalette(c("red","white","black")) #赤と黒でパレットを作成する
Heatmap(heats)
Heatmap(heats,column_title = "PCA for 4-7Yr, normal others tumors", 
        row_title_gp = gpar(fontsize = 20),column_title_gp = gpar(fontsize = 10),
        row_names_gp = gparH(fontsize = 2),column_names_gp = gpar(fontsize = 2), 
        row_dend_width = unit(30, "mm"),column_dend_height = unit(30, "mm"))

###列,行ラベルの設定#####
#列側:column_names_gpオプション
#行側:row_names_gpオプション
#書式はgparオプションで指定;色:col,ラベルサイズ:fontsize;初期値14

Heatmap(heats,column_title = "PCA for 4-7Yr, normal others (absol)", Col=NA
        column_names_gp = gpar(col = c(rep("red", 18), rep("blue", 0)), fontsize = 10),
        row_names_gp = gpar(col = c(rep("red", 240), rep("blue", 362), rep("green",0)), fontsize = 5))

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
