#elastic net
#Elastic Net回帰
#http://tekenuko.hatenablog.com/entry/2017/11/18/214317
#http://highschoolstudent.hatenablog.com/entry/2015/02/08/142354
#https://aizine.ai/ridge-lasso-elasticnet/
#https://stats.biopapyrus.jp/sparse-modeling/glmnet.html
#http://smrmkt.hatenablog.jp/entry/2016/02/21/163507

install.packages("glmnet")
library(glmnet)



#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

#pROCパッケージ
install.packages("pROC")
library(pROC)

#ROCパッケージ
install.packages("ROCR")
library("ROCR")


#MASS使用
#glmnet使用
library(glmnet)

#dataよみこみ
data=read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_05202019_ABOD_for_DrOISHIdata/detailed LV3 testdataset forkaiki 05202019.xlsx",sheet = 2)
anal=data[c(86,27:85)]
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#anal=data[1:1047,32:440]


#characterをfactorに変換
anal$Diagnosis=as.factor(anal$Diagnosis)

#リッジ/Ridge回帰、Lasso回帰、Elastic Net (R - glmnet)準備
#glmnet() 関数を使用するが、目的、説明変数はnumeric型のmatrixでないとエラーが出るので、
#変換しておく。カテゴリー型の変数は直接扱えない。
kaiki.data = as.matrix(data[c(27:85)])
kaiki.target = as.matrix(data[c(86)])

#ディレクトリ変更
setwd("C:/Users/yousu/Google Drive/analyze_pedtumor07112017/8102017") 


#α= 0 とした場合、Ridge回帰となる。
#α= 1 とした場合、Lasso回帰となる。 
#0 < α< 1 の場合、Elastic Netである。

#Ridge回帰を実行
fitRidge1 = glmnet(x=kaiki.data, y=kaiki.target, family="gaussian", alpha=0)
##fitRidge1$betaを実行すると、各Complexity Parameter（λ）におけるパラメータ推定値が確認できる。

#ペナルティと推定値のグラフ。label=TRUE 番号ラベル
plot(fitRidge1, xvar="norm", label=TRUE)
#横軸をλ（log(λ)）とする場合
plot(fitRidge1, xvar="lambda", label=TRUE)

#クロスバリデーションによって、最適なλを決定
#cv.glmnet()で実施。デフォルトの分割数は10分割とのこと。任意の数を指定したい場合は引数nfoldsで指定する。
fitRidgeCV1 <- cv.glmnet( x=kaiki.data, y=kaiki.target, family="gaussian", alpha=0 )
plot(fitRidgeCV1)
#左側の縦点線が、MSEが最小となるときのλの対数
#右側の点線が、上のMSEが最小となるときのMSEの上側1seとなるときのλの対数
min(fitRidgeCV1$cvm)
fitRidgeCV1$lambda.min
log(fitRidgeCV1$lambda.min)

#選ばれたλを、ペナルティと推定値のグラフに描き入れてみる。
plot(fitRidge1, xvar="lambda", label=TRUE)
abline(v=log(fitRidgeCV1$lambda.min), lty=2)

#MSEが最小となる時のλに対応するパラメータを出力するときは以下
coef(fitRidgeCV1, s="lambda.min")
#MSEが最小となるときのMSEの上側1seとなるときのλに対応するパラメータを出力するときは以下。
coef(fitRidgeCV1, s="lambda.1se")
#推定したパラメータによる予測値を求める場合は以下。
pred_fitRidgeCV1 <- predict(fitRidgeCV1, s="lambda.min", newx=kaiki.data)
#glmnetでは、パラメータ推定の際に一度スケーリングが行われ、推定後に元のスケールに戻される処理が行われていることに注意。

#書き出し
write.table(pred_fitRidgeCV1, file="RidgeCV.csv", sep=",")

#roc描画
as.numeric(pred_fitRidgeCV1)
as.vector(pred_fitENCV1)
x=factor(kaiki.target)
ROC <- roc(response=x, predictor=pred_fitRidgeCV1)
plot(1-ROC$specificities, ROC$sensitivities, xlab="1-Specificity", ylab="Sensitivity", type="l", lwd=2) 




#Lasso回帰のあてはめ。

#glmnetで引数alpha=1と指定する。
fitLasso1 <- glmnet( x=kaiki.data, y=kaiki.target, family="gaussian", alpha=1 )
#Lidgeと同様にCVを実行。
fitLassoCV1 <- cv.glmnet( x=kaiki.data, y=kaiki.target, family="gaussian", alpha=1 )
#MSEのプロット。
plot(fitLassoCV1)

#MSEが最小。
log(fitLassoCV1$lambda.min)
#1 SEの位置。
log(fitLassoCV1$lambda.1se)
#ペナルティと推定値のグラフにラインを加えたもの。
plot(fitLasso1, xvar="lambda", label=TRUE)
abline(v=log(fitLassoCV1$lambda.min), lty=2)

#λが最小となる時の、パラメータ推定値。
coef(fitLassoCV1, s="lambda.min")
#予測の場合もRidgeと同様に。
pred_fitLassoCV1 <- predict(fitLassoCV1, s="lambda.min", newx=kaiki.data)

#書き出し
write.table(pred_fitLassoCV1, file="LassoCV.csv", sep=",")

#roc描画
x=factor(kaiki.target)
ROC <- roc(response=x, predictor=pred_fitLassoCV1)
plot(1-ROC$specificities, ROC$sensitivities, xlab="1-Specificity", ylab="Sensitivity", type="l", lwd=2) 




#Elastic Netも同様に実行してみる。

#Elastic Netのαの決め方
alpha <- seq(0.01, 0.99, 0.01)
mse.df <- NULL

for (i in 1:length(alpha)) {
  m <- cv.glmnet(x = kaiki.data, y = kaiki.target, family = "gaussian", alpha = alpha[i])
  mse.df <- rbind(mse.df, data.frame(alpha = alpha[i],
                                     mse = min(m$cvm)))
}

best.alpha <- mse.df$alpha[mse.df$mse == min(mse.df$mse)]


fitEN1 <- glmnet( x=kaiki.data, y=kaiki.target, family="gaussian", alpha=best.alpha )

#CVの実行。
fitENCV1 <- cv.glmnet( x=kaiki.data, y=kaiki.target, family="gaussian", alpha=best.alpha )

#CVによるMSEのプロット。
plot(fitENCV1)
#MSEが最小。
log(fitENCV1$lambda.min)
#ペナルティと推定値のグラフにラインを加えたもの。
plot(fitEN1, xvar="lambda", label=TRUE)
abline(v=log(fitENCV1$lambda.min), lty=2)
#λが最小となる時の、パラメータ推定値。
coef_EN=coef(fitENCV1, s="lambda.min")
write.table(as.matrix(coef_EN), file="coef_EN.csv", sep=",")

#予測値。
pred_fitENCV1 <- predict(fitENCV1, s="lambda.min", newx=kaiki.data)

#書き出し
write.table(pred_fitENCV1, file="ENCV.csv", sep=",")
write.table(as.matrix(coef_EN), file="coef_EN.csv", sep=",")

#------#------#------#------#------#------

#Elastic Netのαの決め方
alpha <- seq(0.01, 0.99, 0.01)
mse.df <- NULL

for (i in 1:length(alpha)) {
  m <- cv.glmnet(x = kaiki.data, y = kaiki.target, family = "gaussian", alpha = alpha[i])
  mse.df <- rbind(mse.df, data.frame(alpha = alpha[i],
                                     mse = min(m$cvm)))
}

best.alpha <- mse.df$alpha[mse.df$mse == min(mse.df$mse)]

m <- cv.glmnet(x = kaiki.data, y = kaiki.target, family = "gaussian", alpha = best.alpha)

best.lambda <- m$lambda.min





#----#----#----#
#ランダムに行を選択
rowdata=nrow(anal)
random_ids=sample(rowdata,rowdata*0.5)

#学習データを作成
anal_training=anal[random_ids,]

#予測データを作成
anal_predicting=anal[-random_ids,]