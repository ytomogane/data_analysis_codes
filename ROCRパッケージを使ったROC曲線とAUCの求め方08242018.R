#RでのROC解析：ROCRパッケージを使ったROC曲線とAUCの求め方
#http://www.nemotos.net/?p=836

install.packages("ROCR")
library("ROCR")

rocdata <- read.table("roc_data.txt")

#ROCR ステップ1: prediction
#ROCRのステップ1はpredictionで、値と属するグループを指定します。
pred <- prediction(rocdata[,1], rocdata[,2])

#ROCR ステップ2: performance
#ROCRのステップ2はperformanceです。ここでは、感度、すなわち真陽性率 (TP/(TP+FN)で定義)と、
#1-特異度、すなわち偽陽性率(FP/(FP+TN)で定義)を求めます。以下のようにタイプします。
perf <- performance(pred, "tpr", "fpr")
#ここでtprはtrue positive rateを、fprはfalse positive rateを意味します。

#ROCR ステップ3: グラフの描画
plot(perf)

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



