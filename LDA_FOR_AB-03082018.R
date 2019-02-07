#FOR ABSOLUTE 03122018
data=read.csv('/Users/yousu/Google_Drive/for_analyze_03132018/4-7years_zscores_analyze03132018_02282018_subject_June2015.csv')
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_02282018/4-7years_zscores_ab_analyze03132018_02282018_subject_June2015.csv')
anal=data[1:1047,32:440]
#167コンポーネント９０パーセント越え
anal=data[1:1232,32:199]
#230コンポーネントで９５パーセント越え
anal=data[1:1232,32:262]
#334コンポーネントで９９パーセント越え
anal=data[1:1232,32:366]

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
