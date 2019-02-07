
# DMwRパッケージのインストール
install.packages("DMwR", quiet = TRUE)


library(DMwR)


#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み


X=0
for(i in 1:5)
{  
  X=X+1
file.name= sprintf("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/LV%01dnormal atlas volume data09062018with diagnosis by Yusuke.xlsx",X,"")
lofdata <- read.xlsx(file.name,
                    sheet="csv")





# LOFスコアの算出
lof.scores <- lofactor(lofdata, 20)
# LOFスコアのヒストグラムのプロット

file.name=sprintf("LV%01dLOF scores.pdf",X,"")

pdf(file.name, width=10, height=10)
file.name=sprintf("LV%01dLOF scores",X,"")
hist(lof.scores, nclass = 20, main = file.name, xlab = "LOF scores")
dev.off()

# LOFの閾値を1.6としたときの外れ値の検出
#is.ol <- lof.scores >= 1.2
#lofdata[is.ol, ]
# 外れ値のLOF
#lof.scores[is.ol]

file.name=sprintf("LOFLV%01d.csv",X,"")
table=lof.scores
write.table(table,file=file.name ,sep=",")
}

#ROC


#Read LOFscore

lofscore <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/LOFresults10082018.xlsx",
                     #                     sheet=1)
                     
                     #rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09062018_ABOD_for_DrOISHIdata/matlabABODresults09062018.xlsx",
                     sheet=1)

X=0
for(i in 1:5)
{  
  X=X+1
  
  #変数を決めておく
  #http://cse.naro.affrc.go.jp/takezawa/r-tips/r/10.html

  J=X 
  #ROCK=paste("lofscore$LV",J,"")
  #as.name(ROCK)
  if (X==1){ROCK=lofscore$LV1}
  if (X==2){ROCK=lofscore$LV2}
  if (X==3){ROCK=lofscore$LV3}
  if (X==4){ROCK=lofscore$LV4}
  if (X==5){ROCK=lofscore$LV5}
  
 
  
  pred <- prediction(ROCK, lofscore$forLOFROCwo)
  
  perf <- performance(pred, "tpr", "fpr")
  
  file.name=sprintf("LV%01dLOFROCcurve.pdf",J)
  
  plot(perf,main=sprintf("LV%01dLOFcurve",J))
  abline(a=0,b=1,lty=2 )
  
  pdf(file.name, width=10, height=10)
  plot(perf,main=sprintf("LOF LV%01dROCcurve",J))
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
  
  file.name=sprintf("LOF LV%01dROC.csv",J)
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
  
  threshold1(ROCK, lofscore$forLOFROCwo)
  threshold2(ROCK, lofscore$forLOFROCwo)
  file.name=sprintf("LOF LV%01dthreshhold.csv",J)
  table=data.frame(Thresh1=threshold1(ROCK, lofscore$forLOFROCwo),Thresh2=threshold2(ROCK, lofscore$forLOFROCwo), AUCresult=auc)
  write.table(table,file=file.name ,sep=",")
}

