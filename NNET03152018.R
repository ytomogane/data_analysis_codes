#neural network

install.packages( "nnet" )
library( nnet )

#SVMのリストから流用
#dataよみこみ
data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
anal=data[1:1047,32:440]

#ランダムに行を選択
rowdata=nrow(anal)
random_ids=sample(rowdata,rowdata*0.5)

#学習データを作成
anal_training=anal[random_ids,]

#予測データを作成
anal_predicting=anal[-random_ids,]

#ksvm関数でトレーニングデータを学習
anal_svm=ksvm(Diagnosis.D.N~.,data=anal_training)

#predict関数で予測データを評価
result_predict<-predict(anal_svm, anal_predicting)


#予測結果と正解との比較
table(result_predict,anal_predicting$Diagnosis.D.N)

#ここからNNET nnet (単一中間層の階層型ニューラルネットパッケージ) 
# nnet関数でNeuralNetworkに学習させる
#sizeで隠れ層のユニット数を規定、typeで出力形式を規定、
anal_nnet=nnet(Diagnosis.D.N~.,data=anal_training,size=2, rang=.1, decay=5e-4, maxit=200)

# 未分類のデータを予測する
result_predict_nnet<-predict(anal_nnet,anal_predicting,type="class")

# 正解と比較
table(result_predict_nnet,anal_predicting$Diagnosis.D.N)






