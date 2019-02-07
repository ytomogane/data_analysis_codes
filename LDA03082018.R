data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/x03142018.csv')

#116コンポーネント９０パーセント越え
anal=data[1:1047,32:147]
#１６１コンポーネントで９５パーセント越え
anal=data[1:1232,32:193]
#２４０コンポーネントで９９パーセント越え
anal=data[1:1232,32:400]

#LDA

#install.packages("MASS") #Packages内にあり


ForLDA1=anal
Z=lda(Diagnosis.D.N~.,data=ForLDA1)

#判別関数を用いた判別得点はコマンド predict(Z)$x で返される。
# $class は各個体が判別されたグループのラベルで、$posterior は
#各個体がどのグループに判別されているかに関する事後確率(0～1)、
#$xは各個体の判別関数得点である。
table(data[,32],predict(Z)$class)
# 定数項式の中のc はグループの平均と判別係数を次ぎのように用いて
#求めることができる。第1列の値は第1判別関数の定数項で、第2列は
#第2判別関数の定数項である。
apply(Z$means%*%Z$scaling,2,mean)
#の個体が誤判別されているかは、次のようなコマンドで追跡することができる。
data.frame(data[,32],predict(Z)$class) 
#判別関数得点をグラフに示すこともできる。次のコマンドで第1判別関数得点
#のグループごとのヒストグラム(分布)が作成される。
plot(Z,dimen=1) 

write.table(predict(Z)$x,file="predict(Z)$x09042017.csv", sep=",")
