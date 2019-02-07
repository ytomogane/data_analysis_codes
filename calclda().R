#konokannsuuha gakuksyuuyou zituyouseihanai


#関数 calclda() は，データセット内の各サンプルに対する判別関数の値を計算する
#さらに，calclda() 関数の中で “scale()” 関数を用いて，判別関数の値（例えば第 1判別関数）を標準化する．その結果，平均値（すべてのワイン・サンプルに対して）は 0 となる．


calclda <- function(variables,loadings)
{
  # データセット中のサンプル数の取得
  as.data.frame(variables)
  numsamples <- nrow(variables)
  # 判別関数を保存するベクトルの作成
  ld <- numeric(numsamples)
  # 変数の数の取得
  numvariables <- length(variables)
  # 各サンプルに対する判別関数の値の計算
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    29
    ld[i] <- valuei
  }
  # 平均が 0 になるように判別関数を標準化:
  ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
  ld <- ld[[1]]
  return(ld)
}


#calclda
calclda(ForLDA1[2:410], Z$scaling[,1])
predict(Z,ForLDA1[2:410])

