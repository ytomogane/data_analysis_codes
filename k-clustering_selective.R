#k-means clustering
#dataはz-score absolute valueを用いる
#parcelとLRratio,whole brain volumeを変数とする
#MASS packageを用いる
data=read.csv('/Users/tomokinn/Google ドライブ/for_analyze09262017_pedMRI/Cluster$A_foranalyze_analyze_subject_mice_absolute_June2015_noJHID_092217_foranalyze_.csv')
#str(data) #data構造の確認

k.clustering=data[1:617,47:447]


#乱数を固定、再現
set.seed(196); runif(5)

#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
#disease.cluster=kmeanspp(k.clustering, k=4)
disease.name=data[1:617,25] 
results=table(disease.name, disease.cluster$cluster) #名前と対応

#第1,2主成分平面上でプロットして、グルーピング
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

results 

#関数 tapply() を使って、データの要素名をクラスター別に表示します
tapply(names(disease.cluster$cluster), disease.cluster$cluster, "unique")

#cluster分類書きだし
write.csv(disease.cluster$cluster,file="Cluster$select1_fromA_analyze_subject_mice_absolute_June2015_noJHID_092217_foranalyze.csv")







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


