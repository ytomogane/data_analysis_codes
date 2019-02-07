#外れ値の検出 UsefulRデータ分析プロセスより
#フォントはUTB-8

# irisデータセットの箱ひげ図のプロット
bp.iris <- boxplot(subset(iris, select = -Species))
# 検出した外れ値
bp.iris$out

# irisデータセットの種類別に各項目の箱ひげ図のプロット
library(ggplot2)
library(reshape2)
# 縦持ち形式のデータに変換
iris.m <- melt(iris)
head(iris.m)
# 箱ひげ図のプロット
p <- ggplot(data = iris.m, aes(x = variable, y = value)) + geom_boxplot() +
  facet_grid(Species ~ .) + theme_bw()
print(p)

# outliersパッケージのインストール

install.packages("outliers", quiet = TRUE)

library(outliers)
set.seed(123)
# 20個の一様乱数に10を追加
x <- c(runif(20), 10)
# Smirnov-Grubbs検定
grubbs.test(x)

# -10もデータに追加
x <- c(x, -10)
# Smirnov-Grubbs検定
grubbs.test(x, type = 11)

# Smirnov-Grubbs検定による外れ値の検出
library(outliers)
# サンプルデータの生成
x <- iris$Sepal.Length
# Smirnov-Grubbs検定
grubbs.test(x)
grubbs.test(x, type = 11)

#depth-based approach
# depthパッケージのインストール
install.packages("depth", quiet = TRUE)

# starsCYGデータセットに対するISODEPTHの実行
library(depth)
install.packages("robustbase", quiet = TRUE)
# starsCYGデータセットのロード
data(starsCYG, package = "robustbase")
head(starsCYG, 3)
# ISODEPTHの実行
isodepth(starsCYG, mustdith = TRUE, xlab = "log.Te", ylab = "log.light")

# ISODEPTHの実行結果の出力
id.sCYG <- isodepth(starsCYG, output = TRUE, mustdith = TRUE, xlab = "log.Te",
                    ylab = "log.light")
# 深さ1の凸包上の点
id.sCYG$Contour1

outputdir <- "data/ELKI"
dir.create(outputdir)
write.table(subset(iris, select = -Species), file.path(outputdir, "iris_for_DBoutlier.tsv"),
            sep = " ", row.names = FALSE, col.names = FALSE)

# ELKIによるDB-外れ値の実行
$ java -jar elki-bundle-0.6.5~20141030.jar KDDCLIApplication n
-algorithm outlier.DBOutlierDetection -dbod.d 1.5 -dbod.p 0.8 n
-dbc.in data/ELKI/iris_for_DBoutlier.tsv -out output/ELKI/DBoutlier

# 出力ディレクトリのファイル一覧
dir("output/ELKI/DBoutlier")
# 結果の確認
res <- read.table("output/ELKI/DBoutlier/db-outlier_order.txt", sep=" ",
                  header=FALSE, colClasses=c("character", rep("numeric", 4), "character"))
head(res)

# DMwRパッケージのインストール
install.packages("DMwR", quiet = TRUE)

# irisデータセットに対するLOFの実行
library(DMwR)
# LOFスコアの算出
lof.scores <- lofactor(iris[, -5], 10)
# LOFスコアのヒストグラムのプロット
hist(lof.scores, nclass = 20, main = "irisのLOFスコア", xlab = "LOFスコア")

# LOFの閾値を1.6としたときの外れ値の検出
is.ol <- lof.scores >= 1.6
iris[is.ol, ]
# 外れ値のLOF
lof.scores[is.ol]


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

java -jar elki-bundle-0.6.5~20141030.jar KDDCLIApplication -algorithm outlier.ABOD n
-dbc.in data/ELKI/mfeat-fac -out output/ELKI/ABOD/mfeat

# 結果が出力されたディレクトリのファイルリスト
dir("output/ELKI/ABOD/mfeat")

library(dplyr)
library(ggplot2)
# データの読み込み
abod.mfeat <- read.table("/Users/tomokinn/Google_drive/for_analyze_07082018_ABOD_for_DrOISHIdata/normal atlas volume data07082018with sexagecorrectionのコピー/abod-outlier_order.txt",
                         sep = " ", as.is = TRUE)
# データのサイズ
dim(abod.mfeat)
# 285列目にABODのスコアが記録されている
head(abod.mfeat$V285)
# スコアを抽出して数値に変換
abod.mfeat <- abod.mfeat %>% mutate(V285 = as.numeric(gsub("abod-outlier=",
                                                           "", V285)))
# スコアのヒストグラムのプロット
p <- ggplot(data = abod.mfeat, aes(x = V285, y = ..count..)) + geom_histogram() +
  scale_x_log10() + xlab("log10(ABODスコア)") + ylab("サンプル数") +
  theme_bw()
print(p)
