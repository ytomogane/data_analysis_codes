#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
#rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/+-2SD on PING08302018.xlsx",
#                     sheet=1)

data <- read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05022019 Pediatric tumor/forR ped tumor data summary 05142019.xlsx",
                     sheet=1)


#LDA

#install.packages("MASS") #Packages内にあり

　
#data=read.csv('/Users/tomokinn/Google ドライブ/AComStaAbu_afterMICE_Text09072017.csv')
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_02282018/PCA results03122018 after absolute/x03122018.csv')
#data=read.csv("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_02282019 Pediatric tumor/ped tumor data summary 04092019 .csv")

ForLDA1=data[1:34,7:23]
Z=lda(ABB~.,data[1:34,7:23]) #CSFのカラムがvariables are collinearであり、外す
label <- data[,7] # 分類ラベル
#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[1:34,7],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
apply(Z$means%*%Z$scaling,2,mean)
#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,7],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。

LD1=predict(Z)$x[,1]
LD2=predict(Z)$x[,2]
LD3=predict(Z)$x[,3]
plot(LD1,LD2,col=label,main='Ped Tumor',cex = 1.5)
legend("topleft", legend=c("PI", "BS","EP","MB"),pch=1,ncol=2,
       col=c("blue", "black","red","green"), cex=0.8)
plot(LD1,LD3,col=label,main='Ped Tumor',cex = 1.5)
legend("topleft", legend=c("PI", "BS","EP","MB"),pch=1,ncol=2,
       col=c("blue", "black","red","green"), cex=0.8)
plot(LD2,LD3,col=label,main='Ped Tumor',cex = 1.5)
legend("topleft", legend=c("PI", "BS","EP","MB"),pch=1,ncol=2,
       col=c("blue", "black","red","green"), cex=0.8)

plot3d(LD1,LD2,LD3,col=rainbow(4)[factor(label)])
writeWebGL()
plot(Z,dimen=3) 

write.table(predict(Z)$x,file="predict(Z).csv", sep=",")
write.table(Z$scaling,file="Zscal.csv", sep=",")
write.table(apply(Z$means%*%Z$scaling,2,mean),file="ZhanbetsuC.csv", sep=",")


#leave one outをためしてみる
Z=lda(ABB~.,data[1:34,7:19],CV=T)
table(data[1:34,7],Z$class)

