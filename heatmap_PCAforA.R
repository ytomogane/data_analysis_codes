data=read.delim('/Users/tomokinn/Google ドライブ/Acoma_for_analyze08028scores.txt')
anal=data[18:35]
heats=as.matrix(anal)
heatsrow=data[,c(6)]
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

Heatmap(heats,column_title = "4-7yr Disease or Normal",
        column_title_side = c("top"),
        column_names_gp = gpar(fontsize = 10,col = c(rep("red", 18), rep("blue", 0))),
        row_names_gp = gpar(fontsize = 2,col = c(rep("red", 85), rep("blue", 240), rep("red",276) 
                                    )))


