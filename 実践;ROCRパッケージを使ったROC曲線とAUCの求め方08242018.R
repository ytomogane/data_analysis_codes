#RでのROC解析：ROCRパッケージを使ったROC曲線とAUCの求め方
#http://www.nemotos.net/?p=836
#実践

install.packages("ROCR")
library("ROCR")

#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_08172018_ABOD_for_DrOISHIdata/matlabABODresults08182018.xlsx",
                        sheet=1)
# データのサイズ
dim(abod.mfeat)





#変数を決めておく
#http://cse.naro.affrc.go.jp/takezawa/r-tips/r/10.html
i=5
Level=sprintf("LV%01d",i)

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
