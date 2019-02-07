#正準判別分析
#kousatuhituyou , tsukuttemitagatukaenai

#https://qiita.com/sbtseiji/items/351b391d78fbbd23dda7

#正準判別分析（改変版） candisc()

candisc<-function(object)  #object = 多変量 lmオブジェクト lm(cbind(X1,X2) ~ Y, data=data)
{
  #### Wilksのラムダの算出関数  
  Wilks <- function(eig, q, df.res) {
    test <- prod(1/(1 + eig))
    p <- length(eig)
    tmp1 <- df.res - 0.5 * (p - q + 1)
    tmp2 <- (p * q - 2)/4
    tmp3 <- p^2 + q^2 - 5
    tmp3 <- ifelse (tmp3 > 0, sqrt(((p * q)^2 - 4)/tmp3), 1)
    c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q, p * q, tmp1 * tmp3 - 2 * tmp2)
  }
  
  #### 結果出力用の関数  
  summary.candisc<-function (object)
  {
    ans<-list(
      "Class Level Information"=object$means,
      "Canonical Discriminant Analysis"=object$eigen,
      "Statistics"=object$stat,
      "Cannonical Variate Coefficients"=object$coeff,
      "Class Means on Canonical Variables"=object$means2)
    
    class(ans) <-c("summary.candisc","listof")
    ans
  }
  
  #### 係数，統計量の算出
  m  <- min(object$rank-1,ncol(object$coefficients))
  
  w  <- resid(object)
  W  <- t(w)%*%w
  t  <- resid(update(object,~1))
  
  T  <- t(t)%*%t 
  B  <- T-W
  We <- eigen(W,symmetric=TRUE)
  W1 <- solve(t(We$vectors %*% diag(sqrt(We$values))))
  ev <- eigen( t(W1)%*% B %*% W1)
  cv <- W1 %*% ev$vectors * sqrt(nrow(object$model)-object$rank)
  cv <- cv[,1:m]
  
  e <- ev$values
  corsq  <- e[1:m]/(1+e[1:m])
  prop <- e[1:m]/sum(e[1:m])
  cum <- cumsum(prop)
  emat <-data.frame(CanCorr = sqrt(corsq), EigenValue=e[1:m], Proportion=prop, Cumulative=cum)
  
  if (m>1){
    colnames(cv) <-colnames(cv,do.NULL=FALSE,prefix="CAN")
    rownames(cv) <-colnames(object$coefficients,do.NULL=FALSE)
  }
  
  STAT<-matrix(c(rep(0,m*5)),ncol=5)
  for(i in 1:m){
    lambda <- Wilks(e[i:NROW(e)],object$rank-i,object$df.residual)
    q      <- lambda[2]
    df1    <- lambda[3]
    df2    <- lambda[4]
    pval   <- round(pf(q,df1,df2,lower=F),8)
    STAT[i,] <- c(lambda[1],q,df1,df2,pval)
  }
  colnames(STAT) <-c("Lambda","ApproxF","NumDF","DenDF","Prob")
  STAT<-as.data.frame(STAT)
  
  #### クラス情報
  f_table<-as.vector(table(object$model[,-1]))
  
  df<-data.frame(Class=object$model[,-1],object$fitted.values)
  meanTable <- aggregate(.~Class , df,mean)
  rownames(meanTable)<-meanTable[,1]
  meanTable$Class <- NULL
  meanTable$Frequency<-f_table
  meanTable$Proportion<-f_table/sum(f_table)
  
  
  #### 判別関数のクラス別平均
  df<-data.frame(Class=object$model[,-1], (object$fitted.values+object$residuals) %*% cv)
  canMean <- aggregate(.~ Class, df, mean)
  rownames(canMean)<-canMean[,1]
  canMean$Class <- NULL
  
  (summary.candisc(structure(list( "eigen"=emat,
                                   "stat"=STAT,
                                   "coeff"=cv,
                                   "means"=meanTable,
                                   "means2"=canMean))))
}


#candisc()関数には，多変量のlmオブジェクトを渡します。多変量lmオブジェクトは，次のような書式で作成します。
#model <- lm( cbind(Petal.Length, Sepal.Length, Petal.Width, Sepal.Width) ~ Species, data= iris)


data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/04102018LDA for 3rd cluster 1 and 3/thirdkmean3Cluster040522018forCandisc.xlsx",sheet = 1)

ForLDA1=data[c(25,35:443)]
Z=lm(Diagnosis.D.N.0isNand1isD~.,data=ForLDA1)
