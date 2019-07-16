#nipals 04022019

#https://cran.r-project.org/web/packages/nipals/vignettes/nipals_algorithm.pdf


install.packages("nipals")
library("nipals", lib.loc="~/R/win-library/3.5")

#test

B <- matrix(c(50, 67, 90, 98, 120, 55, 71, 93, 102, 129, 65, 76, 95, 105, 134,
              50, 80, 102, 130, 138,
              60, 82, 97, 135, 151,
              65, 89, 106, 137, 153,
              75, 95, 117, 133, 155), ncol=5, byrow=TRUE)
B2 <- B
B2[1,1] <- B2[2,1] <- NA
m0 <- svd(scale(B)) # center and scale
require("nipals")
m1 <- nipals::nipals(B2, gramschmidt=FALSE) 
m2 <- nipals::nipals(B2, gramschmidt=TRUE)

#nipalsはPCAに用いるとされており、LDAに用いるには不十分の可能性がある。
#他の方法を模索する
#多重代入法、miceは以前試した。代表的なパッケージとされている。
#その前に、はたしてもともと存在しない値に欠損値補完をして良いのか、と言う問題がある。

