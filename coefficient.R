#Correlation

#Spearman, Kendall

#for_MAC
#data=read.csv('/Users/tomokinn/Google_drive/for_analyze_02062018/Analyze???Similarity_evaluation_Test_sheet_withResult_02192018_kang.csv',stringsAsFactors = FALSE)
 
#for_WIN
#data=read.csv('C:/Users/yousu/Google_Drive/Analyze???Similarity_evaluation_Test_sheet_withResult_02192018_kang.csv')
#for.distance=data[1:1033,41:453]

data=read.csv('C:/Users/yousu/Desktop/AnalyzeSimilarity_evaluation_Test_sheet_withResult_02192018 _kawasaki.csv',stringsAsFactors = FALSE)


x=10
y=11

for (i in 1:14) 
{
 
  Euclidean.data=data[x,3:7]
  Kendall.data=data[y,3:7]
  Base.data=1:5
  Euclidean.data=as.numeric(Euclidean.data)
  Kendall.data=as.numeric(Kendall.data)
#spearman
  print(cor.test(Base.data,Euclidean.data, method="spearman"))

#kendall
  print(cor.test(Base.data,Euclidean.data, method="kendall"))

#spearman
  print(cor.test(Base.data,Kendall.data, method="spearman"))

#kendall
print(cor.test(Base.data,Kendall.data, method="kendall"))

  x=x+2
  y=y+2
    }
