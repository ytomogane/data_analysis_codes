#svm leave one out

#SVM e1071
#https://data-science.gr.jp/implementation/iml_r_svm.html
#leave one out
#http://akusenkutou.hatenablog.com/entry/2016/03/04/174159



#e1071を用いる
install.packages("e1071", repos="http://cran.ism.ac.jp/")
library(e1071)

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)


#dataよみこみ
data=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05202019_ABOD_for_DrOISHIdata/detailed LV3 testdataset forSVM 05202019.xlsx",sheet = 2)
anal=data[c(27:86)]
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#anal=data[1:1047,32:440]

#characterをfactorに変換
anal$Diagnosis=as.factor(anal$Diagnosis)

#ランダムに行を選択
samples=sample(nrow(anal),nrow(anal)*0.96)

#学習データを作成
#予測データを作成

anal_training=anal[samples,]
anal_predicting=anal[-samples,]


#トレーニングデータセットで分類器を作成するが．分類器作成のために必要なパラメーターの最適化を行う
#tunecontrol に指定してある cross および cross=10 は 10-fold クロスバリデーションにて最適パラメーターを探索するということを示している．

tune=tune.svm(Diagnosis ~ .,data=anal_training, gamma=10^(seq(-5, 5, 0.1)), cost=10^(seq(-2, 2, 0.1)), tunecontrol=tune.control(sampling="cross", cross=10))

#確認
tune

write.csv(tune$best.parameters,file="C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017/tune_best.parameters_e1071.csv")

#これらの値を用いて分類器を作成する
#classifier=svm(Diagnosis ~ ., data=anal_training, method="C-classification", kernel="radial", gamma=###, cost=###)
classifier=svm(Diagnosis ~ ., data=anal_training, method="C-classification", kernel="radial", gamma=0.01995262, cost=0.6309573,
               cross=nrow(anal_training))
#leave one out (cross=nrow(anal_training))を行った。

#結果、Total Accuracy: 80.93995 

#作成した分類器 classifier を用いて，テストデータセットの分類は以下のように行う
predict(classifier, anal_predicting)



#以下のコマンドでテーブルに
table(predict(classifier, anal_predicting), anal_predicting$Diagnosis)

#正確度 (accuracy) は以下のように計算する．以上の場合では全て正解しているので正確度は1となる．
sum(diag( table(predict(classifier, anal_predicting), anal_predicting$Diagnosis)))/sum(table(predict(classifier, anal_predicting), anal_predicting$Diagnosis))

#----------#

#cross option不使用 leave one out
#http://akusenkutou.hatenablog.com/entry/2016/03/04/174159

i=1
results<-list() 
while(i<10){
  class=1
  correct=0
 while(class<=nrow(anal_training)){  #Leave-one-out cross validation
     model<-svm(Diagnosis~.,data= anal_training[-class,])#, gamma=0.01995262, cost=0.6309573)
     if(predict(model,anal_training[class,])==anal_training[class, ncol(anal_training)]) correct<-correct+1
    class=class+1} # /LOOCV
   tot.accuracy=correct/nrow(anal_training)*100
   cat("accuracy =", tot.accuracy, "\n")
   results=rbind(results,tot.accuracy)
   i<-i+1}

