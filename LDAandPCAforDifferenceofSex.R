#LDA, PCA


#install.packages("MASS") #Packages内にあり


#性別の比較

data=read.csv('C:/Users/yousu/Google Drive/for_analyza_12202017/analyze MaleFemale 01192018__4-7yr_subject_June2015.csv')
SEXSEX=data[1:1085,40:462]
ForLDASEX=data[1:1085,41:462]
Z=lda(Gender.1~.,data=ForLDASEX)


#lda(formula, data)
#formulaには、「グループの識別変数~変数」のように記述する。


#PCA

library(devtools)
install_github("ggbiplot", "vqv")
install.packages("FactoMineR")
library(FactoMineR)
source("http://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
library(pcaMethods)
listPcaMethods()


pcIr <- pca(ForLDASEX, method="nipals", nPcs=10, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
Sex <- data[,40]
label <- as.factor(data[,40])
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)
label
write.table(scores(pcIr),file="Acom_for_analyze08025scores.csv", sep=",")
write.table(loadings(pcIr),file="Acom_for_analyze08025loadings.csv", sep=",")
write.table(summary(pcIr),file="Acom_for_analyze08025summary.csv", sep=",")




read.c