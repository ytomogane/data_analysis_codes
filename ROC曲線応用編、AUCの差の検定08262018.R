#ROC曲線応用編、AUCの差の検定
#http://rcommanderdeigakutoukeikaiseki.com/ROC_AUC_NRI_and_IDI.html

#dataをGLM.1 GLM.2に入れて比較

GLM.1=glm(forROC~LV5,family=binomial(logit), data=rocdata)
summary(GLM.1)

#さて、ここからは下記スクリプトをコピーアンドペーストしてR Console上で実行するだけです。
#※回帰分析のモデル式の名前 （下記スクリプトの赤字のGLM.1の部分)は適宜変更して下さい。

library(pROC)
ROC <- roc(response=rocdata$forROC, predictor=GLM.2$fitted) 
plot(1-ROC$specificities, ROC$sensitivities, xlab="1-Specificity", ylab="Sensitivity", type="l", lwd=2)
abline(a=0,b=1,lty=2) # ROC曲線の描画
CI<-ci.auc(ROC, conf.level=0.95) # AUCの信頼区間
cutoff <- coords(ROC, x="best", ret=c("threshold", "sensitivity", "specificity", "ppv", "npv"), best.method="closest.topleft")
c.point <- cutoff[1] # モデル上のカットオフ値を格納
beta<- coef(GLM.1) # ロジスティックモデルの回帰係数（対数オッズ比）を格納
cutoff.variable <- (log(c.point/(1-c.point))-beta[1])/beta[2] # 元の変量のカットオフ値を計算
#ここから結果
ROC$auc #ROC曲線の曲線下面積AUC（C統計量とも呼ばれる）
CI #曲線下面積AUCの95%信頼区間
c.point # 回帰分析モデル上のカットオフ値
cutoff.variable #説明変数が1変数の場合の最適カットオフ値（説明変数が複数ある場合は無視）
cutoff[2:5] 
#sensitivityは 感度（%表記なら100倍）
#specificityは特異度（%表記なら100倍）
#ppvは陽性的中率（positive predictive value)（%表記なら100倍）
#npvは陰性的中率NPV（negative predictive value）（%表記なら100倍）

#以上でROC曲線の描出と、様々な統計量の計算が終了


#AUCの差の検定

GLM.2=glm(forROC~LV4,family=binomial(logit), data=rocdata)
summary(GLM.2)

library(pROC)
ROC <- roc(response=rocdata$forROC, predictor=GLM.2$fitted) 
par(new=T)
plot(1-ROC$specificities, ROC$sensitivities, xlab="1-Specificity", ylab="Sensitivity", type="l", lwd=2, col="blue")
abline(a=0,b=1,lty=2 ) # ROC曲線の描画 col="色"で色を指定
CI<-ci.auc(ROC, conf.level=0.95) # AUCの信頼区間
cutoff <- coords(ROC, x="best", ret=c("threshold", "sensitivity", "specificity", "ppv", "npv"), best.method="closest.topleft")
c.point <- cutoff[1] # モデル上のカットオフ値を格納
beta<- coef(GLM.2) # ロジスティックモデルの回帰係数（対数オッズ比）を格納
cutoff.variable <- (log(c.point/(1-c.point))-beta[1])/beta[2] # 元の変量のカットオフ値を計算
#ここから結果
ROC$auc #ROC曲線の曲線下面積AUC（C統計量とも呼ばれる）
CI #曲線下面積AUCの95%信頼区間
c.point # 回帰分析モデル上のカットオフ値
cutoff.variable #説明変数が1変数の場合の最適カットオフ値（説明変数が複数ある場合は無視）
cutoff[2:5] 
#sensitivityは 感度（%表記なら100倍）
#specificityは特異度（%表記なら100倍）
#ppvは陽性的中率（positive predictive value)（%表記なら100倍）
#npvは陰性的中率NPV（negative predictive value）（%表記なら100倍）


#GLM.1とGLM.2を使ったROC曲線のAUCの差を検討

ROC1 <- roc(response=rocdata$forROC, predictor=GLM.1$fitted)
ROC2 <- roc(response=rocdata$forROC, predictor=GLM.2$fitted)
roc.test(ROC1, ROC2, method="delong", alternative="two.sided")


#NRI（net reclassification improvement）と
#IDI（integrated discrimination improvement）の算出方法

library(PredictABEL)
predRisk1 <- GLM.1$fitted
predRisk2 <- GLM.2$fitted
value<-as.vector(quantile(GLM.1$fitted))
cutoff <- c(0,value[2],value[3],value[4],1)
reclassification(data=rocdata, cOutcome=11, predrisk1=predRisk1, predrisk2=predRisk2, cutoff=cutoff)

#decision curve analysis (DCA)のほうがいいかも
