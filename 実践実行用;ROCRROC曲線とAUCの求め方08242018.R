##RでのROC解析：ROCRパッケージを使ったROC曲線とAUCの求め方
#http://www.nemotos.net/?p=836
#実践

install.packages("ROCR")
library("ROCR")

#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
#rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/+-2SD on PING08302018.xlsx",
#                     sheet=1)

rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/matlabABODresults09282018.xlsx",
                      sheet=1)

# データのサイズ
dim(rocdata)

X=0
for(i in 1:6)
{  
X=X+1

#変数を決めておく
#http://cse.naro.affrc.go.jp/takezawa/r-tips/r/10.html
#i=41
J=X 
#ROCK=paste("rocdata$LV",J,"")
#as.name(ROCK)
if (X==1){ROCK=rocdata$LV1}
if (X==2){ROCK=rocdata$LV2}
if (X==3){ROCK=rocdata$LV3}
if (X==4){ROCK=rocdata$LV4}
if (X==5){ROCK=rocdata$LV5}
if (X==6){ROCK=rocdata$LV41 
          J=41}

pred <- prediction(ROCK, rocdata$forROCwo)

#小さいほど、異常になるのでtp->fn, fp->tn
#perf <- performance(pred, "tpr", "fpr")
perf <- performance(pred, "tnr", "fnr")
file.name=sprintf("LV%01dROCcurve.pdf",J)

#sensitvity specificity反転

perf@x.name <- 'False positive rate'
perf@y.name <-'True positive rate'

  
plot(perf,main=sprintf("LV%01dROCcurve",J))
abline(a=0,b=1,lty=2 )

pdf(file.name, width=10, height=10)
plot(perf,main=sprintf("ABOD LV%01dROCcurve",J))
abline(a=0,b=1,lty=2 )
dev.off()

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc


table <- data.frame(Cutoff=unlist(pred@cutoffs),
                    TP=unlist(pred@tp), FP=unlist(pred@fp),
                    FN=unlist(pred@fn), TN=unlist(pred@tn),
                    Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),
                    Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),
                    Accuracy=((unlist(pred@tp)+unlist(pred@tn))/nrow(rocdata))
)

max(table$Accuracy)

file.name=sprintf("ABOD LV%01dROC.csv",J)
write.table(table,file=file.name ,sep=",")


#threshold1 <- function(prediif ct, response) {
#  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
#  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
#  df[which.max(df$sens + df$spec), "cut"]
#}
#threshold2 <- function(predict, response) {
#  r <- pROC::roc(response, predict)
#  r$thresholds[which.max(r$sensitivities + r$specificities)]
#}

threshold1(ROCK, rocdata$forROCwo)
threshold2(ROCK, rocdata$forROCwo)
file.name=sprintf("ABOD LV%01dthreshhold.csv",J)
table=data.frame(Thresh1=threshold1(ROCK, rocdata$forROCwo),Thresh2=threshold2(ROCK, rocdata$forROCwo), AUCresult=auc)
write.table(table,file=file.name ,sep=",")
}

