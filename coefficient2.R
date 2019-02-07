#Correlation

#Spearman, Kendall

#for_MAC
data=read.csv('/Users/tomokinn/Google_drive/for_analyze_02062018/Analyze???Similarity_evaluation_Test_sheet_withResult_02192018_kang.csv',stringsAsFactors = FALSE)
 
#for_WIN
#data=read.csv('C:/Users/yousu/Google_Drive/Analyze???Similarity_evaluation_Test_sheet_withResult_02192018_kang.csv')
#for.distance=data[1:1033,41:453]


Euclidean.data=data[37,3:7]
Kendall.data=data[36,3:7]
Base.data=1:5
Euclidean.data=as.numeric(as.character(Euclidean.data))
Kendall.data=as.numeric(Kendall.data)

#spearman
cor.test(Base.data,Euclidean.data, method="spearman")

#kendall
cor.test(Base.data,Euclidean.data, method="kendall")

#spearman
cor.test(Base.data,Kendall.data, method="spearman")

#kendall
cor.test(Base.data,Kendall.data, method="kendall")

