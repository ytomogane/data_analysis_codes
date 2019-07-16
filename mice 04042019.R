#mice


#多重代入法（multiple imputation）

data2=read.csv('/Users/tomokinn/Desktop/data_analyze_of_tumors6:12:2017/z-score_Ped2000_tumor_data_05232017_combined_M2corrected_withTumorROI_7172017.csv')
#anal=data2[1:834,45:445]
anal=data2[,2:13]

# miceをinstall
#install.packages("mice")
#library(mice)


#欠損値の全体を知る
md.pattern(anal)

#md.pairs関数で2項目の欠損有無の組み合わせごとに件数を表示する

md.pairs(anal)

#欠損値を含むデータの可視化のために作られたパッケージ
#install.packages("VIM")
#library(VIM)
#aggr_plot <- aggr(data,
#                  col=c('navyblue','red'),
#                  numbers=TRUE, sortVars=TRUE, labels=names(data),
#                  cex.axis=.7, gap=3,
#                  ylab=c("Histogram of missing data","Pattern"))


#データを補完する
tempData <- mice(anal,
                 m=1, # refers to the number of imputed datasets. Five is the default value.
                 #                 maxit=50,
                 meth='pmm', # refers to the imputation method, pmm: predictive mean matching
                 #                 seed=500
)

summary(tempData)
#補完したデータを確認するには
tempData$imp$Ozone

completedData <- complete(tempData,1) # 選択した番号

#補完後のデータの確認を行う
#Scatter plot
#まずはxyplot()でscatter plotで全体像を把握します。
#赤が補完したデータで青が本来計測されたデータです。
#だいたい分布が一緒になっており最もらしい補完データであることがわかります。

#library(lattice) 
xyplot(tempData,pch=430,cex=1)


write.csv(completedData,file="z-score_mice_Ped2000_tumor_data_05232017_combined_M2corrected_withTumorROI_7172017.csv")
