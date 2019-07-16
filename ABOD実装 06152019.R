#ABOD translate from matlab



#repmat and function, isnan and var function
install.packages("pracma")
library(pracma)
install.packages("SparkR")
library(SparkR)



#ABOF
abod <- function(X){
  n=dim(X)
  d=dim(X)[2]
  n=dim(X)[1]
  res=matrix(0, nrow=n, ncol=1)
  
  for(i in 1:n){
    Y=X-repmat(as.numeric(X[i,]),n,1) #repmatにて繰り返し行列作成(matlabと同様)
    Temp=Y*Y
    s=rowSums(Temp)
    s=repmat(s,1,d)
    Y=Y/s
    Y[is.na(Y)]=0 #NAを0に変換
    res[i]=compAngvar(Y)
  }
  res
}

#compAngvar

compAngvar <- function(Y){
  Tmp=as.matrix(Y)%*%t(Y) #matrix同士の掛け算になる
  s=Tmp
  s[upper.tri(s,diag=TRUE)]=0
  v=s[s!=0]
  var(v)
}

#threshold
#解釈
#Threshold1;感度と特異度の合計が最も高いもの、Threshold2；pROC packageで求めたcut off値を用いて決定

threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}
threshold2 <- function(predict, response) {
  r <- pROC::roc(response, predict)
  r$thresholds[which.max(r$sensitivities + r$specificities)]
}


#参考

#\と/の置き換え
pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

#pathPrep()実行でクリップボードのスラッシュが変換される。Functionにはいっている。
#例；"C:\Users\yousu\Google_Drive\for_analyze_03152018\June2015.csv"
#実行後
#例；[1] "\"C:/Users/yousu/Google_Drive/for_analyze_03152018/June2015.csv\""クリップボード内にある。
#パスコピーpathPrep()実行、ペースト。これでいける

pathPrep()

#test
X=data[1:50,28:60]
