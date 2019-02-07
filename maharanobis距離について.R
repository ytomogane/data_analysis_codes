#maharanobis距離について

m.dat=colMeans(k.clustering)
v.dat=var(k.clustering)
mahalanobis(k.clustering,m.dat,v.dat)
hist(mahalanobis(k.clustering,m.dat,v.dat))

write.csv(x=v.dat,file="v.dat.csv")