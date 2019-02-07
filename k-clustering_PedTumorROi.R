#k-means clustering
#dataはz-score absolute valueを用いる
#parcelとLRratio,whole brain volumeを変数とする
#MASS packageを用いる
data2=read.csv('/Users/tomokinn/Google ドライブ/for_analyzePedTumorMRI10162017/forAnalyze_Ped2000_tumor_data_05232017_combined_M2corrected_withTumorROI_7172017_10132017 .csv')
anal=data2[,2:16]
#str(data) #data構造の確認

ktumor.clustering=anal


#乱数を固定、再現
set.seed(196); runif(5)

#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
diseasetumor.cluster=kmeans(x=ktumor.clustering, centers=3)
#disease.cluster=kmeanspp(k.clustering, k=3)
diseasetumor.name=data2[,1] 
results=table(diseasetumor.name, diseasetumor.cluster$cluster) #名前と対応

#第1,2主成分平面上でプロットして、グルーピング
clusplot(ktumor.clustering, diseasetumor.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

results 

#関数 tapply() を使って、データの要素名をクラスター別に表示します
tapply(names(diseasetumor.cluster$cluster), diseasetumor.cluster$cluster, "unique")

#cluster分類書きだし
write.csv(diseasetumor.cluster$cluster,file="Cluster$for_analyze_Ped2000_tumor_data_05232017_combined_M2corrected_withTumorROI_7172017.csv")


