#LDA for 3rd clustering 04232018 with L/R ratio absolute value


#LDA

install.packages("MASS") #Packages内にあり
library("MASS", lib.loc="C:/Program Files/R/R-3.4.4/library")


data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/04102018LDA for 3rd cluster 1 and 3/thirdkmean3Cluster040522018.xlsx",sheet = 1)

#L/R ratioのみを用いる。
ForLDA1=data[c(25,310:442)]
Z=lda(Diagnosis.D.N~.,data=ForLDA1)

#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[,25],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
Z$means%*%Z$scaling
#これで、それぞれの「グループの平均」と「判別係数」の線形結合がだせる
#平均はそれぞれのグループに属する個数による。平均を計算し、定数項を出す。
#個体数が同一なら、これが使える。
#apply(Z$means%*%Z$scaling,2,mean)

#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,25],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。
plot(Z,dimen=1) 

write.table(predict(Z)$x,file="predict(Z)$x04232018withLR.csv", sep=",")
write.table(data.frame(data[,25],predict(Z)$class),file="predict(class)$x04232018withLR.csv", sep=",")

write.table(Z$scaling,file="scaling04232018withLR.csv" ,sep=",")
write.table(Z$means%*%Z$scaling,file="C04232018withLR.csv" ,sep=",")
write.table(predict(Z)$posterior,file="posterior04232018withLR.csv" ,sep=",")


#Next step omit DD and LDA by z-score volume not absolute


data=read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_04032018 k-mean clustering/04232018LDA for 3rd cluster 1 and 3 onlyLR/predict(class)$x04232018withLR.xlsx",sheet = 1)

#volumedataのみを用いる。
ForLDA1=data[c(2,37:311)]
Z=lda(Diagnosis.D.N~.,data=ForLDA1)

#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[,2],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
Z$means%*%Z$scaling
#これで、それぞれの「グループの平均」と「判別係数」の線形結合がだせる
#平均はそれぞれのグループに属する個数による。平均を計算し、定数項を出す。
#個体数が同一なら、これが使える。
#apply(Z$means%*%Z$scaling,2,mean)

#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,25],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。
plot(Z,dimen=1) 

write.table(predict(Z)$x,file="predict(Z)$x04232018NextLDA.csv", sep=",")
write.table(data.frame(data[,2],predict(Z)$class),file="predict(class)$x04232018NextLDA.csv", sep=",")

write.table(Z$scaling,file="scaling04232018NextLDA.csv" ,sep=",")
write.table(Z$means%*%Z$scaling,file="C04232018NextLDA.csv" ,sep=",")
write.table(predict(Z)$posterior,file="posterior04232018NextLDA.csv" ,sep=",")

#
#https://www1.doshisha.ac.jp/~mjin/R/Chap_18/18.html

# 判別関数を用いて、学習データについて判別を行った結果は関数 predict を用いて返す。関数 predict は次の値を返す。
#①　 predict()$class 
#②　 predict()$posterior 
#③　 predict()$x
#$class は各個体が判別されたグループのラベルで、$posterior は各個体がどのグループに判別されているかに関する事後確率(0～1)、$xは各個体の判別関数得点である。#
#学習データにおける判別結果は次のような表で確認することができる。