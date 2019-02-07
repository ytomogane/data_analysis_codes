#ROC
ForROC=read.csv('/Users/tomokinn/Google ドライブ/forROC_LD1andClass09042017.csv')
ResultsROC=roc(formula=Class~LD1, data=ForROC)
## ci()で信頼区間をつけられます。
ci(ResultsROC, of = "auc", method = "delong")  # DeLong
ci(ResultsROC, of = "auc", method = "boot")    # bootstrap
## of = "thresholds" をつけると任意のカットオフでの感度特異度の95%信頼区間がだせます。
ci(ResultsROC, of = "thresholds")
## plot()するとグラフになります。
## 何も考えずに書くとpROCはX軸を一般的ではない(けど確かにわかりやすい)方法でかきます。
## legacy.axes = TRUE を与えるとX軸が 1 - specificityになります。
plot(ResultsROC)
