# discriminant analysis example, Summit Cr. data, two variables, two groups
library(MASS)
library(lattice) # for plots
sumcr <- read.csv("C:/Users/yousu/Downloads/sumcr.csv")
attach(sumcr)

# recode Reach into two levels, Grazed and Ungrazed
Grazed <- as.integer(Reach)
Grazed[Grazed==3] <- 1
Grazed <- as.factor(Grazed)
levels(Grazed) <- c("G","U")
plot(WidthWS, DepthWS, type="n")
text(WidthWS, DepthWS, label=as.character(Grazed))

# discriminant analysis, coefficients and scores
Grazed_lda1 <- lda(Grazed ~ WidthWS + DepthWS, method="moment")
Grazed_lda1

plot(Grazed_lda1)

Grazed_dscore <- predict(Grazed_lda1, dimen=1)$x
cor(cbind(WidthWS, DepthWS, Grazed_dscore))
