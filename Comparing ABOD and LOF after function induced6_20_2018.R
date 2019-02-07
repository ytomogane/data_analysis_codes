#Comparing ABOD and LOF

#In this section, I show a performance comparison between LOF and naive version of ABOD when 
#applied to high dimensional data sets. A toy data set is created to have 20 rows and 200 columns.


# Normal data are generated from normal distribution with mean=0 and sd=1
set.seed(12345)
test <- matrix(data = rnorm(n = 20 * 200, mean = 0, sd = 1), nrow = 20)
dim(test)

# Randomly select 5 rows to inject errors
set.seed(12345)
inject.row <- sample(1:dim(test)[1], size = 5)
inject.row

# Outliers are generated from a uniform distribution with min=0 and max=1
set.seed(12345)
test[inject.row, ] <- runif(n = length(inject.row) * dim(test)[2], min = 0, 
                            max = 1)

#To perform the LOF algorithm:
# Load the package Rlof to perform the LOF algorithm
library(Rlof)

# A wide range of k is selected to do the outlier detection Since the toy
# data set has 20 rows, max k is 19
res.lof <- lof(data = test, k = 2:19)

# Check how many true outliers can be unveiled in the top 5 candidates
res.lof.count <- foreach(i = 1:dim(res.lof)[2], .combine = c) %dopar% {
  score <- res.lof[, i]
  score.order <- order(score, decreasing = T)[1:length(inject.row)]
  length(which(score.order %in% inject.row))/length(inject.row)
}
res.lof.count

#To perfrom the naive version of ABOD algorithm, i.e., all the rest of data are used as reference
ABOD.score <- func.ABOD(data = test, k.size = dim(test)[1] - 1, kNN.value = dim(test)[1] - 
                          1, kNN.final = dim(test)[1] - 1)
ABOD.score.order <- order(ABOD.score, decreasing = F)[1:length(inject.row)]
length(which(ABOD.score.order %in% inject.row))/length(inject.row)

