#LDA
#Bgroupは症例数が変数以下。

data=read.delim('/Users/tomokinn/Google ドライブ/BComStaAbu_afterMICE09092017.txt')
ForLDA1=data[c(6,17:293)]
Z=lda(Diagnosis.D.N~.,data=ForLDA1)
#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[,6],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
apply(Z$means%*%Z$scaling,2,mean)
#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,6],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。
plot(Z,dimen=1) 


ZZ=data.frame(data[,1:6],predict(Z)) 
write.table(ZZ,file = "LDA_of_Bmice_completedata0911.csv",sep=",")
write.table(Z$scaling,file = "scalinglimitedBmice_completedata0911.csv",sep=",")
#prior	the prior probabilities used.
#means	the group means.
#scaling	a matrix which transforms observations to discriminant functions, 
#normalized so that within groups covariance matrix is spherical.
#svd	the singular values, which give the ratio of the between- and 
#within-group standard deviations on the linear discriminant variables. 
#Their squares are the canonical F-statistics.
#N	The number of observations used.
#call	The (matched) function call.
#whole brain and Fimbria_L.R were omitted from the list.
