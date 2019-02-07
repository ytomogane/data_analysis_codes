
#k-means clustering 03162018
#dataはz-score absolute valueを用いる
#parcelとLRratio,whole brain volumeを変数とする
#MASS packageを用いる
#str(data) #data構造の確認


#Clustering1 03222018
data=read.csv('/Users/yousu/Google_Drive/for_analyze_03152018/4-7years_zscores_to_ab_analyze03132018_02282018_subject_June2015.csv')
k.clustering=data[1:1047,33:440]
disease.name=data[1:1047,23] 
#乱数を固定、再現
set.seed(5); runif(5)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="firstkmean3Cluster030222018.csv")
write.csv(results,file="results of firstkmean3Cluster030222018.csv")

#clustering1
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/4-7y z-score to ab 04052018_subject_June2015_noJHID_022118_newM2_MergedWithVolume.xlsx",sheet=1)
disease.name=data[1:1047,23] 
data=data.matrix(data)
k.clustering=data[1:1047,31:439]
#乱数を固定、再現
set.seed(7); runif(7)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="firstkmean3Cluster04052018.csv")
write.csv(results,file="results of firstkmean3Cluster04052018.csv")

#Clustering2
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/first3cluster/firstkmean3Cluster04052018.xlsx",sheet = 1)
disease.name=data[1:1004,25] 
data=data.matrix(data)
k.clustering=data[1:1004,34:440]
#乱数を固定、再現
set.seed(6); runif(7)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="secondkmean3Cluster04052018.csv")
write.csv(results,file="results of secondkmean3Cluster04052018.csv")


#Clustering3
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/second3cluster/secondkmean3Cluster04052018.xlsx",sheet = 1)
disease.name=data[1:884,25] 
data=data.matrix(data)
k.clustering=data[1:884,34:440]
#乱数を固定、再現
set.seed(6); runif(5)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="thirdkmean3Cluster040522018.csv")
write.csv(results,file="results of thirdkmean3Cluster040522018.csv")



#Clustering4
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/third3cluster/thirdkmean3Cluster040522018.xlsx",sheet = 1)
disease.name=data[1:871,25] 
data=data.matrix(data)
k.clustering=data[1:871,34:440]
#乱数を固定、再現
set.seed(6); runif(5)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="forthkmean4Cluster040522018.csv")
write.csv(results,file="results of forthkmean4Cluster040522018.csv")


#Clustering5
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/forth3cluster/forthkmean4Cluster040522018.xlsx",sheet = 1)
disease.name=data[1:813,25] 
data=data.matrix(data)
k.clustering=data[1:813,34:440]
#乱数を固定、再現
set.seed(6); runif(5)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=4)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="fifthkmean4Cluster040522018.csv")
write.csv(results,file="results of fifthkmean4Cluster040522018.csv")

#Clustering6
data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/fifth4cluster/fifthkmean4Cluster040522018.xlsx",sheet = 1)
disease.name=data[1:761,25] 
data=data.matrix(data)
k.clustering=data[1:761,34:440]
#乱数を固定、再現
set.seed(5); runif(7)
#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="sixthkmean4Cluster040522018.csv")
write.csv(results,file="results of sixthkmean4Cluster040522018.csv")

#乱数を固定、再現
set.seed(5); runif(5)

#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=3)
#disease.cluster=kmeanspp(k.clustering, k=4)
#disease.name=data[1:1013,25] 
results=table(disease.name, disease.cluster$cluster) #名前と対応

results

#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#ラベル付けトライ中
#pointLabel(x=sample$PAR, y=sample$SE, labels=rownames(sample))

#関数 tapply() を使って、データの要素名をクラスター別に表示します
tapply(names(disease.cluster$cluster), disease.cluster$cluster, "unique")

#cluster分類書きだし
write.csv(disease.cluster$cluster,file="forthkmean4Cluster030222018.csv")
write.csv(results,file="results of forthkmean4Cluster030222018.csv")







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



#距離を測定
k.clusterigdistance=dist(x=k.clustering, method = "euclidean", diag = TRUE, upper = FALSE)
write.csv(x=as.matrix(k.clusterigdistance), file = "distance.csv")

#中心からの距離最後の4行が中心点
withcenter.kcluster=rbind(disease.cluster$centers,k.clustering)
fromclustercenterdistance=dist(x=withcenter.kcluster, method = "euclidean", diag = TRUE, upper = FALSE)
write.csv(x=as.matrix(fromclustercenterdistance), file = "distance.csv")

