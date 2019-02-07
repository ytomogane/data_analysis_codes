#ABOD実装 functionから
#http://rajadatamining.blogspot.com/2013/08/using-r-to-perform-angle-based-outlier.html
#使用
install.packages("ggplot2", quietly = TRUE)
install.packages("foreach", quietly = TRUE)
install.packages("doMC", quietly = TRUE)
install.packages("plyr", quietly = TRUE)
install.packages("class", quietly = TRUE)
install.packages("Rlof", quietly = TRUE)
library(foreach)
library(doMC)
library(plyr)
library(ggplot2)
library(class)
registerDoMC(cores = 8)


#SNN
func.SNN <- function(data.scaled, kNN.value, kNN.final) {
  require(Rlof)  #Since function distmc is attached in this package
  # Calculate the distance among observations
  dist.mat <- as.matrix(distmc(x = data.scaled, method = "euclidean", diag = F))
  # Create a matrix to contain the index of kNN observations
  res.dist.order <- foreach(i = 1:dim(dist.mat)[2], .combine = rbind) %dopar% 
  {
    dist.order <- order(dist.mat[, i], decreasing = F)[1:(kNN.value + 
                                                            1)]
    dist.order.upd <- dist.order[-1]
  }
  # Create a matrix to store the number of common nearest neighbors
  nn.mat <- matrix(data = NA, nrow = dim(data.scaled)[1], ncol = dim(data.scaled)[1])
  for (j in 1:dim(data.scaled)[1]) {
    index.to.compare <- c(1:dim(data.scaled)[1])[-j]
    for (m in 1:length(index.to.compare)) {
      nn.mat[j, index.to.compare[m]] <- length(intersect(res.dist.order[j, 
                                                                        ], res.dist.order[index.to.compare[m], ]))
    }
  }
  diag(nn.mat) <- 0
  # Create a matrix to contain the candidate index to consider (using ordered
  # SNN)
  cand.mat <- foreach(p = 1:dim(nn.mat)[1], .combine = rbind) %dopar% {
    vec <- as.numeric(nn.mat[p, ])
    vec.order <- order(vec, decreasing = T)
    vect.output <- vec.order[1:kNN.final]
  }
  return(cand.mat)
}

#ABOD
func.ABOD <- function(data, k.size, kNN.value, kNN.final) {
  candidate.mat <- func.SNN(data.scaled = data, kNN.value = kNN.value, kNN.final = kNN.final)
  res.ABOD <- foreach(i = 1:dim(data)[1], .combine = rbind) %dopar% {
    res.var.k <- foreach(m = 1:length(k.size), .combine = cbind) %do% {
      k.select <- candidate.mat[i, ]
      combine <- t(combn(x = k.select, m = 2))
      
      angles <- foreach(j = 1:dim(combine)[1], .combine = cbind) %dopar% 
      {
        v.1 <- data[combine[j, 1], ] - data[i, ]
        v.2 <- data[combine[j, 2], ] - data[i, ]
        theta <- round(acos(sum(v.1 * v.2)/(sqrt(sum(v.1 * v.1)) * 
                                              sqrt(sum(v.2 * v.2)))), digits = 2)
        return(theta)
      }
      return(ifelse(k.size[m] == 2, angles, round(var(as.numeric(angles)), 
                                                  digits = 4)))
    }
  }
}

#comparing ABOD vs LOF


