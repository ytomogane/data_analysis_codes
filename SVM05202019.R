#SVM

#kernlabを用いる
install.packages( "kernlab" )
library( kernlab )

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)


#dataよみこみ
data=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05202019_ABOD_for_DrOISHIdata/detailed LV3 testdataset forSVM 05202019.xlsx",sheet = 2)
anal=data[c(86,27:85)]
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#anal=data[1:1047,32:440]

#ランダムに行を選択
rowdata=nrow(anal)
random_ids=sample(rowdata,rowdata*0.5)

#学習データを作成
anal_training=anal[random_ids,]

#予測データを作成
anal_predicting=anal[-random_ids,]

#ksvm関数でトレーニングデータを学習
#anal_svm=ksvm(Diagnosis.D.N~.,data=anal_training)
#kernelをRBFにするにはkernel="rbfdot"を追加
anal_svm=ksvm(Diagnosis~.,data=anal_training,kernel="rbfdot",C=1,cross=3)


#predict関数で予測データを評価
#result_training<-predict(anal_svm, anal_training)
result_predict<-predict(anal_svm, anal_predicting)


#予測結果と正解との比較
#table(result_training,anal_training$Diagnosis.D.N)
table(result_predict,anal_predicting$Diagnosis)

