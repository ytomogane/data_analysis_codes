#k-meansのcluster数の決定
#Gap統計量
#cluster packageを用いる
library("cluster", lib.loc="C:/Program Files/R/R-3.4.3/library")
#kmenas：K-means法
#K.max：クラスタ数の最大値
#B：モンテカルロ法のブートストラップ回数
#verborse：処理状況を表示するかどうか

#data=read.csv('/Users/tomokinn/Google ドライブ/for_analyze09262017_pedMRI/A_analyze_subject_mice_absolute_June2015_noJHID_092217_foranalyze.csv')
#k.clustering=data[1:834,45:445]

#data=read.csv('/Users/tomokinn/Google ドライブ/for_analyze09262017_pedMRI/Cluster$A_foranalyze_analyze_subject_mice_absolute_June2015_noJHID_092217_foranalyze_.csv')
#k.clustering=data[1:617,47:447]

#Cluster1Cluster2and3
#data2=read.csv('/Users/tomokinn/Google ドライブ/for_analyze_JHU_all_data2017-2019/for_analyze09262017_pedMRI/multipleCluster1Cluster2and3_fromA_analyze_subject_mice_absolute_June2015_noJHID_092217_foranalyze.csv')
#data=read.csv('/Users/yousu/Google_Drive/for_analyze_02282018/4-7years_zscores_analyze03012018_02282018_subject_June2015.csv')

#k.clustering=data2[,2:13]
#k.clustering=data2[1:590,46:446]

#k.clustering=data[1:1232,32:439]


data=read.csv('/Users/yousu/Google_Drive/for_analyze_03152018/4-7years_zscores_to_ab_analyze03132018_02282018_subject_June2015.csv')


k.clustering=data[1:1047,33:440]



result <- clusGap(k.clustering, kmeans, K.max = 10, B = 100, verbose = interactive())
result
plot(result)

#このとき、求めるクラスタ数kは
#gap(k) ≧ gap(k+1) - SE.sim(k+1)
#となる最小のkである。