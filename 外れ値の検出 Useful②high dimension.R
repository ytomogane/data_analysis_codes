#高次元の外れ値
library(ggplot2)
set.seed(123)
# 次元のリスト
dims <- c(1:9, 10 * (1:9), 100 * (1:10))
# 算出する統計量
stats <- c("min", "mean-sd", "mean", "mean+sd", "max")
# 発生させる点の個数
N <- 2000
# 各次元に対して算出した統計量を格納する行列
ans <- matrix(NA, length(dims), length(stats), dimnames = list(dims, stats))
# 各次元に対して発生させた点間の距離の統計量を算出する
for (d in dims) {
  message("d=", d)
  # 点を発生させる(N * d の行列)
  x <- replicate(d, runif(N))
  # 点間の距離を算出する
  x.dist <- as.matrix(dist(x))
  xd <- x.dist[upper.tri(x.dist)]
  # 点間の距離の統計量を算出する
  xd.min <- min(xd, na.rm = TRUE)
  xd.mean <- mean(xd, na.rm = TRUE)
  xd.max <- max(xd, na.rm = TRUE)
  xd.sd <- sd(xd, na.rm = TRUE)
  # 統計量を格納する(次元間を比較できるようにsqrt(dim)で割る)
  ans[as.character(d), ] <- c(xd.min, xd.mean - xd.sd, xd.mean, xd.mean +
                                xd.sd, xd.max)/sqrt(d)
}
# 次元と統計量の関係をプロットする
install.packages(reshape2)
library(reshape2)

ans.m <- melt(ans)
head(ans.m)
ans.m$Var2 <- factor(ans.m$Var2, levels = rev(stats))
p <- ggplot(data = ans.m, aes(x = Var1, y = value, group = Var2, colour = Var2)) +
  geom_point(aes(shape = Var2)) + geom_line() + theme_bw() %+replace%
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank()) + xlab("次元") + ylab("正規化した距離")
print(p)

# Multiple Featuresデータセットのダウンロード
inputdir <- "data/ELKI"
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/mfeat/mfeat-fac",
              file.path(inputdir, "mfeat-fac")) 

#ELKIでのパラメータ
java -jar elki-bundle-0.6.5~20141030.jar KDDCLIApplication -algorithm outlier.ABOD n
-dbc.in data/ELKI/mfeat-fac -out output/ELKI/ABOD/mfeat

# 結果が出力されたディレクトリのファイルリスト
dir("C:/Users/yousu/Google_Drive/for_analyze_08012018_ABOD_for_DrOISHIdata/PCA for normal atlas volume data06072018 sexagecorrection z-scoreonlyLRzscore/PCA1-PCA20/abod-outlier_order.txt")

install.packages("dplyr")
library(dplyr)
library(ggplot2)

# データの読み込み
abod.mfeat <- read.table("C:/Users/yousu/Google_Drive/for_analyze_08142018_ABOD_for_DrOISHIdata/LV4without low parcels normal atlas volume data06072018.xlsx",
                         sep = " ", as.is = TRUE)
# データのサイズ
dim(abod.mfeat)
# 218列目にABODのスコアが記録されている
head(abod.mfeat$V196)
# スコアを抽出して数値に変換
abod.mfeat <- abod.mfeat %>% mutate(V196 = as.numeric(gsub("abod-outlier=",
                                                           "", V196)))
# スコアのヒストグラムのプロット
p <- ggplot(data = abod.mfeat, aes(x = V196, y = ..count..)) + geom_histogram() +
  scale_x_log10() + xlab("log10(ABOD scores)") + ylab("sample numbers") +
  theme_bw()
print(p)
