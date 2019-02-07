#study about SVM

install.packages("e1071")
library("e1071")

## a simple example
library(MASS)
data(cats)
m <- svm(Sex~., data = cats)
plot(m, cats)

## more than two variables: fix 2 dimensions
data(iris)
m2 <- svm(Species~., data = iris)
plot(m2, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))



#Rでは有名な libsvm の wrapper である {kernlab} や {e1071}, {KlaR} でSVMを使えます。
#今回は {kernlab} を使って Iris libsvm
#の versicolor と virginica の2クラス分類を行ってみます。

#kernlab::ksvm の type には分類問題の場合は SVC (Support Vector Classification) , 
#回帰問題の場合は SVR (Support Vector Regression) を指定します。


require(kernlab)

set.seed(1500)

d <- data.frame(iris[51:150,3:4], y = as.character.factor(iris[51:150,5])) # versicolor and virginica
d$y <- setattr(d$y,"levels", c("versicolor", "virginica"))

iris.ksvm <- ksvm(
  y ~ ., # y is Species
  data = d,
  kernel = "rbfdot",
  kpar = list(sigma=0.2), # kernel param sigma
  C = 5, # margin param C
  cross = 3
)

print(iris.ksvm)

plot(iris.ksvm, data = d[,1:2])

table(d$y, predict(iris.ksvm, d[,1:2]))
#             versicolor virginica
# versicolor         47         3
# virginica           3        47