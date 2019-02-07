#PCAforNewData
#03062018

data=read.csv('/Users/yousu/Google_Drive/for_analyze_08012018_ABOD_for_DrOISHIdata/normal atlas volume data06072018 sexagecorrection to %volume to z-score with LRratiozscore for ELKI.csv',stringsAsFactors=F)
anal=data[1:191,11:416]
#anal=data[1:191,11:286]

pcIr <- pca(anal, method="nipals", nPcs=110, cv="q2")
pc1 <- scores(pcIr)[,1]
pc2 <- scores(pcIr)[,2]
pc3 <- scores(pcIr)[,3]
pc4 <- scores(pcIr)[,4]
label <- as.factor(data[,3])
percent <- summary(pcIr)[2,2] * 100
plot(pc1, pc2, col=label)
scores(pcIr)
loadings(pcIr)
summary(pcIr)

write.csv(scores(pcIr),file='scores.csv')
write.csv(loadings(pcIr),file='loading.csv')
write.csv(summary(pcIr),file='summary.csv')


#component no kazu ni genkaigaaru 110 made

#PCAを使用
library("FactoMineR", lib.loc="~/R/win-library/3.4")
Fpca=PCA(anal, ncp=1)
Fpca
#自動で値が調整されてしまうらしい
#https://cis-jp.blogspot.com/2012/09/blog-post_5.html

#pcrompを使用
#https://cis-jp.blogspot.com/2012/09/blog-post_5.html
Xpca=prcomp(anal)
SXpca=summary(Xpca)
#syuseibunn %
SXpca$importance
#component
Xpca$rotation
#hyouzyunnhennsa
Xpca$sdev
#summary
str(Xpca)
#tokuten
Xpca$x


write.csv(Xpca$x,file='x.csv')
write.csv(Xpca$rotation,file='rotation.csv')
write.csv(Xpca$sdev,file='sdev.csv')
write.csv(SXpca$importance,file='summary.csv')
