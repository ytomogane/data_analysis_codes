#ABOD leave one out

#ABOD function needed

#ROCパッケージ
install.packages("ROCR")
library("ROCR")

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)



#repmat and function, isnan and var function
install.packages("pracma")
library(pracma)
install.packages("SparkR")
library(SparkR)




#dataよみこみ
data=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_06202019_ABOD_for_DrOISHIdata/forR detailed LV3 testdataset for ABOD LOO 06252019.xlsx",sheet = 1)
anal=data[c(1:798),c(30:89)]
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#anal=data[1:1047,32:440]

#Xに数値代入　診断一覧をDIAに代入

X=anal[,1:59]
n=dim(X)[1]
abResult=abod(X)  #全体のABOF、これと比較する        
LOOres=matrix(0, nrow=n, ncol=1) 　#結果格納用リスト
AUC=matrix(0, nrow=n, ncol=1) 

X=anal[,1:59]
DIA=anal[,60]



for (i in 1:n) {
  Xs=X
  Xs[i,]=NA
  Xs=na.omit(Xs)
  Xr=abod(Xs)
  Ds=DIA
  Ds[i]=NA
  Ds=na.omit(Ds)
  Ds=as.character(Ds)
  TH1=threshold1(Xr,Ds)
  LOOres[i]=TH1

  #ROC prediction
  x=Xr
  y=Ds
  yy="diagnosis"
  pred <- prediction(x,y)
  #ROC performance
  perf <- performance(pred, "tpr", "fpr")
  
  #filename変数生成
  file.name=sprintf("%sABOD.pdf",i)
  
  setwd("C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017")
  
  #ROCR ステップ3: グラフの描画
  plot(perf)
  pdf(file.name, width=10, height=10)
  plot(perf)
  dev.off()
  
  
  #AUCの算出
  auc.tmp <- performance(pred,"auc")
  auc <- as.numeric(auc.tmp@y.values)
  AUC[i]=auc

  #正診率＝(TP+TN)/総数
  #感度(真陽性率)＝TP/(TP+FN)
  #特異度＝TN/(FP+TN)
  setwd("C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017")
  table <- data.frame(Cutoff=unlist(pred@cutoffs),
                      TP=unlist(pred@tp), FP=unlist(pred@fp),
                      FN=unlist(pred@fn), TN=unlist(pred@tn),
                      Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),
                      Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),
                      Accuracy=((unlist(pred@tp)+unlist(pred@tn))/nrow(x))
  )
  file.name=sprintf("%sABOD.csv",i)
  write.table(table,file=file.name ,sep=",")
  
  
}

#書き出し
setwd("C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017")
write.table(abResult, file="abResult.csv", sep=",")
write.table(LOOres, file="LOOres.csv", sep=",")

#summary table 作成
summary <- data.frame(LOOabodresult=LOOres,
                    ABOD=abResult, AUC=auc)
write.table(summary, file="Summary.csv", sep=",")





----.-----.-----.-----.----
  
  
  
#ROCパッケージ
install.packages("ROCR")
library("ROCR")


#roc描画
#ROCR ステップ1: prediction
#ROCRのステップ1はpredictionで、値と属するグループを指定します。
x=Xr
#結果を指定
y=Ds
yy="diagnosis"
pred <- prediction(x,y)

#ROCR ステップ2: performance
#ROCRのステップ2はperformanceです。ここでは、感度、すなわち真陽性率 (TP/(TP+FN)で定義)と、
#1-特異度、すなわち偽陽性率(FP/(FP+TN)で定義)を求めます。以下のようにタイプします。
perf <- performance(pred, "tpr", "fpr")
#ここでtprはtrue positive rateを、fprはfalse positive rateを意味します。

#filename変数生成
file.name=sprintf("%sABOD.pdf",i)

#ROCR ステップ3: グラフの描画
plot(perf)
pdf(file.name, width=10, height=10)
plot(perf)
dev.off()

#AUCの算出
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#正診率の算出
#ここからさらに一歩踏み込んで、正診率を求めたいと思います。

#正診率、感度、特異度は以下で定義されます。

#正診率＝(TP+TN)/総数
#感度(真陽性率)＝TP/(TP+FN)
#特異度＝TN/(FP+TN)
table <- data.frame(Cutoff=unlist(pred@cutoffs),
                    TP=unlist(pred@tp), FP=unlist(pred@fp),
                    FN=unlist(pred@fn), TN=unlist(pred@tn),
                    Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),
                    Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),
                    Accuracy=((unlist(pred@tp)+unlist(pred@tn))/nrow(y))
)

#Accuracyがもっとも高いところを見つけるには、
max(table$Accuracy)

file.name=sprintf("%sABOD.csv",i)
write.table(table,file=file.name ,sep=",")


#感度特異度曲線
#感度と特異度の曲線も簡単に書けます。predictionまで行った後に、次のようにします。
#感度、特異度の曲線がsens-spec-curve.pngという名前で保存されます。
perf <- performance(pred, "sens", "spec")
png(file.name=sprintf("%ssens-spec-curve.png",i))
plot(perf)
dev.off()

#Threshhold
#function
threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}
threshold2 <- function(predict, response) {
  r <- pROC::roc(response, predict)
  r$thresholds[which.max(r$sensitivities + r$specificities)]
}
#実際
threshold1(y,x)
threshold2(y,x)










#-----#-----#-----#----#----#
#ROCR ステップ1: prediction
#ROCRのステップ1はpredictionで、値と属するグループを指定します。
select(rocdata,)
pred <- prediction(rocdata$LV5, rocdata$forROC)


#ROCR ステップ2: performance
#ROCRのステップ2はperformanceです。ここでは、感度、すなわち真陽性率 (TP/(TP+FN)で定義)と、
#1-特異度、すなわち偽陽性率(FP/(FP+TN)で定義)を求めます。以下のようにタイプします。
perf <- performance(pred, "tpr", "fpr")
#ここでtprはtrue positive rateを、fprはfalse positive rateを意味します。

#filename変数生成
file.name=sprintf("LV%01dROCcurve.pdf",i)


#ROCR ステップ3: グラフの描画
plot(perf)
pdf(file.name, width=10, height=10)
plot(perf)
dev.off()

#AUCの算出
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#正診率の算出
#ここからさらに一歩踏み込んで、正診率を求めたいと思います。

#正診率、感度、特異度は以下で定義されます。

#正診率＝(TP+TN)/総数
#感度(真陽性率)＝TP/(TP+FN)
#特異度＝TN/(FP+TN)
table <- data.frame(Cutoff=unlist(pred@cutoffs),
                    TP=unlist(pred@tp), FP=unlist(pred@fp),
                    FN=unlist(pred@fn), TN=unlist(pred@tn),
                    Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),
                    Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),
                    Accuracy=((unlist(pred@tp)+unlist(pred@tn))/nrow(rocdata))
)

#Accuracyがもっとも高いところを見つけるには、
max(table$Accuracy)

file.name=sprintf("LV%01dROC.csv",i)
write.table(table,file=file.name ,sep=",")


#感度特異度曲線
#感度と特異度の曲線も簡単に書けます。predictionまで行った後に、次のようにします。
#感度、特異度の曲線がsens-spec-curve.pngという名前で保存されます。
perf <- performance(pred, "sens", "spec")
png("LV3sens-spec-curve.png")
plot(perf)
dev.off()

#Threshhold
#function
threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}
threshold2 <- function(predict, response) {
  r <- pROC::roc(response, predict)
  r$thresholds[which.max(r$sensitivities + r$specificities)]
}
#実際
threshold1(rocdata$LV5, rocdata$forROC)
threshold2(rocdata$LV5, rocdata$forROC)
#------#------#------#------#------#------