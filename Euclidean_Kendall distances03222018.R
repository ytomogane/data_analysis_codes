#Eucltidean and Kendall distance

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

#Eucridean distance

data=read.xlsx('C:/Users/yousu/Google_Drive/for_LDDM_02122018/forMatlab_analyze/leftHemisphericVer3.xlsx',sheet = 3)
for.distance=data[2:6,3:100]
#データフレームを数値行列に変換
for.distance=data.matrix(for.distance)
#as.data.frame.table(for.distance)

#
#data=read.csv('C:/Users/yousu/Google_Drive/for_LDDM_02122018/forMatlab_analyze/BifrontalAtrophy.csv')
#for.distance=data[1:5,3:424]

distance.each=dist(x=for.distance, method = "euclidean", diag = TRUE, upper = FALSE)
write.xlsx(x=as.matrix(distance.each), file = "euclidan_leftHemisphericVer3.xlsx")



#Kendall distance

#Kendall package
#nstall.packages("RMallow")
#library("RMallow", lib.loc="~/R/win-library/3.4")

#data tableに変換
as.data.frame.table(for.distance)
AResult=AllKendall(for.distance,for.distance)

write.xlsx(AResult,file="Kendall_leftHemisphericVer3.xlsx")
