#LDA

#install.packages("MASS") #Packages内にあり


#data=read.csv('/Users/tomokinn/Google ドライブ/AComStaAbu_afterMICE_Text09072017.csv')
data=read.csv('/Users/yousu/Google_Drive/for_analyze_02282018/PCA results03122018 after absolute/x03122018.csv')

ForLDA1=data[c(7,18:424)]
Z=lda(Diagnosis.D.N~.,data=ForLDA1)

#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[,7],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
apply(Z$means%*%Z$scaling,2,mean)
#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,6],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。
plot(Z,dimen=1) 

write.table(predict(Z)$x,file="predict(Z)$x09042017.csv", sep=",")
