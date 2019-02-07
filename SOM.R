install.packages("kohonen")


# Load the kohonen package
require(kohonen)

data=read.csv('/Users/tomokinn/Google ドライブ/for_analyze_JHU_all_data2017-2019/for_analyze09262017_pedMRI/A_analyze_subject_June2015_noJHID_092217_foranalyze.csv')
completeData=data[1:834,45:445]

gr=somgrid(xdim = 15, ydim = 15, topo = c("hexagonal"))

somresult=som(as.matrix(completedData),gr,rlen=1000,keep.data = TRUE,alpha = c(0.05, 0.01))
summary(somresult)
data_train <- completedData

plot(somresult,type="codes",shape=c("straight"))
lab.cod=data[,23]
lab.color<- as.numeric(data[,23])

plot(somresult, type="mapping",labels=lab.cod, col=lab.color,shape=c("straight"))
#bgcol = cm.colors(nrow(somresult$codes))[rank(somresult$codes[, i])]
plot(somresult, type="count",labels=lab.cod,main="Node Counts",palette.name=coolBlueHotRed,shape=c("straight"))

#dists <- unit.distances(gr)
#nodeそれぞれの評価をheatmap化
#plot(somresult, type="property",property =getCodes(somresult,1)[,1],main=colnames(getCodes(somresult,1))[1], palette.name=coolBlueHotRed)
#色の設定
#coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

#Clusteringについて
#install.packages("tempR") pretty_paletteに使うが、結局使っていない
#library(tempR)
### use hierarchical clustering to cluster the codebook vectors
#plot(somresult, type="mapping",labels=lab.cod, col=lab.color,main="Clusters",bgcol = som.CL)
#som.CL<-cutree(hclust(object.distances(somresult,"codes")),4)
#add.cluster.boundaries(somresult,som.CL)


#以下参考資料

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process.
data_train_matrix <- as.matrix(scale(data_train))

# Create the SOM Grid - you generally have to specify the size of the
# training grid prior to training the SOM. Hexagonal and Circular
# topologies are possible
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix,
                 grid=som_grid,
                 rlen=500,
                 alpha=c(0.05,0.01),
                 keep.data = TRUE )


書き方# 色と記号の設定
COL <- c("#FF3300", "#339966", "#0041FF")
BGC <- c("#FFFF99", "#76E4A6", "#B4EBFA")
SP <- as.numeric(iris[,5])
LBL <- c("se","ve","vi")
# マップ
plot(iris.som, type="mapping", labels=LBL[SP], col=COL[SP], bg=BGC[1])
# ユニットごとの個体数
plot(iris.som, type="counts")
# 収束の様子
plot(iris.som, type="changes")
# ユニットをクラスタリングして色づけ
BGC <- c("#FFFF99", "#76E4A6", "#B4EBFA")
iris.som.dist <- dist(iris.som$codes)^2
CLST <-cutree(hclust(iris.som.dist,"ward"),3)
plot(iris.som, type="mapping", labels=LBL[SP], col=COL[SP], bgcol=BGC[CLST])

