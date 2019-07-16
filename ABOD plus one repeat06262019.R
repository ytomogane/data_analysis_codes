#ABOD実装
#JHH plus PING ABOD (sex age correctionはJHH normalのCoeffを用いて行う)



#repmat and function, isnan and var function
install.packages("pracma")
library(pracma)
install.packages("SparkR")
library(SparkR)


#ROCパッケージ
install.packages("ROCR")
library("ROCR")

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)


#dataよみこみ
TData=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_06202019_ABOD_for_DrOISHIdata/forR detailed LV3 testdataset for ABOD LOO 06252019.xlsx",sheet = 1)
TFIG=TData[c(1:798),c(30:89)]
TEData=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_06202019_ABOD_for_DrOISHIdata/for R PING data forABODtest LV3 training dataset06272019.xlsx",sheet = 1)
TEFIG=TEData[c(1:183),c(18:76)]



X=TFIG[,1:59]
n=dim(X)[1]
Y=TEFIG[,1:59]
m=dim(Y)[1] 
abResult=abod(X)  #全体の基準となるABOF、これと比較する。
#１例加えた分散と比較することになるが、外部データ評価のために用いる  

Testres=matrix(0, nrow=m, ncol=1) 　#結果格納用リスト
#AUC=matrix(0, nrow=n, ncol=1) 

X=TFIG[,1:59]
DIA=TFIG[,60]

#基準となるROCcurveを引く
#ROC prediction
x=abResult
y=DIA
yy="main diagnosis"
pred <- prediction(x,y)
#ROC performance
perf <- performance(pred, "tpr", "fpr")

#filename変数生成
file.name="mainABOD"

setwd("C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017")

#ROCR ステップ3: グラフの描画
plot(perf)
pdf(file.name, width=10, height=10)
plot(perf)
dev.off()


#AUCの算出
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
AUC=auc

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
file.name="mainABOD.csv"
write.table(table,file=file.name ,sep=",")
TH1=threshold1(x,y)
Summary=data.frame(threshold=TH1,AUC=auc)
write.table(Summary, file="Summary main ABOD .csv", sep=",")


PING=matrix(0, nrow=m, ncol=1) 　#結果格納用リスト
#THH=matrix(0, nrow=m, ncol=1)
#AUC=matrix(0, nrow=n, ncol=1) 

#X=TData[,1:59]
#DIA=TData[,60]

i=1
for (i in 1:m) {
  Xs=X
  Xs[n+1,]=Y[i,]
#  Xs=na.omit(Xs)
  Xr=abod(Xs)
  PING[i]=Xr[n+1]
#  Xr[n+1]=NA
#  Xr=na.omit(Xr)
#  Ds=DIA
#  Ds[i]=NA
#  Ds=na.omit(Ds)
#  Ds=as.character(Ds)
#  THH[i]=threshold1(Xr,Ds)

  
}
Testres=data.frame(PINGABOD=PING)
write.table(Testres, file="PINGthreshold.csv", sep=",")






#-------.----.----.------.-------.-------.-------
  





PING=matrix(0, nrow=m, ncol=1) 　#結果格納用リスト
THH=matrix(0, nrow=m, ncol=1)
#AUC=matrix(0, nrow=n, ncol=1) 

X=TData[,1:59]
DIA=TData[,60]


for (i in 1:m) {
  Xs=X
  Xs[n+1,]=Y[i,]
  #  Xs=na.omit(Xs)
  Xr=abod(Xs)
  PING[i]=Xr[n+1]
  Xr[n+1]=NA
  Xr=na.omit(Xr)
  Ds=DIA
  #  Ds[i]=NA
  #  Ds=na.omit(Ds)
  #  Ds=as.character(Ds)
  THH[i]=threshold1(Xr,Ds)
  
  #ROC prediction
  x=Xr
  y=Ds
  yy="diagnosis"
  pred <- prediction(x,y)
  #ROC performance
  perf <- performance(pred, "tpr", "fpr")
  
  #filename変数生成
  file.name=sprintf("%sPINGABOD.pdf",i)
  
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
  file.name=sprintf("%sPINGABOD.csv",i)
  write.table(table,file=file.name ,sep=",")
  
  
}
Testres=data.frame(PINGABOD=PING,Threshold=THH)
write.table(Testres, file="PINGthreshold.csv", sep=",")




