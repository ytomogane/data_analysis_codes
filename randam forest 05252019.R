#randam forest
#https://www.slideshare.net/hamadakoichi/randomforest-web


install.packages("randomForest") 
library(randomForest) 

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)


#dataよみこみ
data=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05202019_ABOD_for_DrOISHIdata/detailed LV3 testdataset forSVM 05202019.xlsx",sheet = 2)
anal=data[c(86,27:85)]
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#anal=data[1:1047,32:440]


#characterをfactorに変換
anal$Diagnosis=as.factor(anal$Diagnosis)


#ランダムに行を選択
rowdata=nrow(anal)
random_ids=sample(rowdata,rowdata*0.5)

#学習データを作成
anal_training=anal[random_ids,]

#予測データを作成
anal_predicting=anal[-random_ids,]

#Diagnosisを分類変数としてforest生成
forest = randomForest(Diagnosis ~ .,data=anal_training)

# Forestを用いた予測の実行
pred.forest <- predict(forest, newdata = anal_predicting, type = "class")

# 解との比較 
table(pred.forest, anal_predicting[,1]) 

#tuningする
#https://tjo.hatenablog.com/entry/2013/12/24/190000

tuneRF(anal[,-8],anal[,8],doBest=T)


rf=randomForest(Diagnosis ~ .,data=anal,mtry=8)
print(rf)
