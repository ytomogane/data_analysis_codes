
#\と/の置き換え
pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

#pathPrep()実行でクリップボードのスラッシュが変換される。Functionにはいっている。
#例；"C:\Users\yousu\Google_Drive\for_analyze_03152018\June2015.csv"
#実行後
#例；[1] "C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/forR ped tumor data summary 03152019.xlsx"
#クリップボード内にある。
#パスコピーpathPrep()実行、ペースト。これでいける

pathPrep()



#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)




#PCAforPedTumor

source("http://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
library(pcaMethods)
listPcaMethods()
#rocrも必要



data=read.xlsx('C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/forR ped tumor data summary 03152019.xlsx')
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
write.table(scores(pcIr),file="PCAscores.csv", sep=",")
write.table(loadings(pcIr),file="PCAloadings.csv", sep=",")
write.table(summary(pcIr),file="PCA summary.csv", sep=",")
write.table( completeObs(pcIr),file="PCA nipals data.csv", sep=",")


#欠損値ありでclusteringできるか。
#nipals resultを利用



#k-meansのcluster数の決定
#Gap統計量
#cluster packageを用いる

#kmenas：K-means法
#K.max：クラスタ数の最大値
#B：モンテカルロ法のブートストラップ回数
#verborse：処理状況を表示するかどうか


data=read.csv("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/PCA k-mean data results/03202019/PCA nipals data.csv")

k.clustering=data[1:27,2:16]



result <- clusGap(k.clustering, kmeans, K.max = 10, B = 100, verbose = interactive())
result
plot(result)

#このとき、求めるクラスタ数kは
#gap(k) ≧ gap(k+1) - SE.sim(k+1)
#となる最小のkである。

#LDAはどうかとご意見

#k-means clustering 
#dataはnipals resultを用いる
#MASS packageを用いる
#str(data) #data構造の確認

#Clustering
data=read.csv("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/PCA k-mean data results/03202019/PCA nipals data.csv")
k.clustering=data[1:27,2:16]
disease.name=data[1:27,1] 
#乱数を固定、再現
set.seed(5); runif(5)

#関数 kemeans() を使って、K平均法を使った非階層型クラスタリングを実行します
#clusteringに同じ名前は使えないので番号を振った方が良い
disease.cluster=kmeans(x=k.clustering, centers=4)
results=table(disease.name, disease.cluster$cluster) #名前と対応
results
#第1,2主成分平面上でプロットして、グルーピング
#cluster package使用
#library(cluster)
#library(RColorBrewer) #カラーパレットbrewer.palを用いる

clusplot(k.clustering, disease.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#cluster分類書きだし
write.csv(disease.cluster$cluster,file="k-means pedtumor.csv")
write.csv(results,file="results ofk-means pedtumor.csv")

