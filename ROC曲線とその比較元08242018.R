#ROC曲線とその比較
#https://rpubs.com/kaz_yos/1713

## ROC曲線を描いてみます。
## Introductory Statistics with R 2nd ed. (Peter Dalgaard)のデータを使っています。
## 1版は和訳 Rによる医療統計学 (岡田 昌史) もあります。

## 使い方
## この記載をすべてコピーして手元のRのエディタに貼付ける。
## Windows版では左上 [ファイル] → [新しいスクリプト]でエディタ。
## Mac版では真っ白い紙の形のアイコンでエディタ。
## 実行する行、あるいは範囲を選択して
## WindowsであればCtrl＋R、MacであればCommand＋Returnで実行。

## インストール用の関数定義
## rownames(installed.packages()でインストールされているパッケージの一覧
## PKG %in% ... という表現は...の中にPKGが含まれればTRUEを返します。!をつけて評価を逆転。
inst <- function (PKG) {
  if (!(PKG %in% rownames(installed.packages()))) {
    install.packages(pkgs = PKG, dependencies = TRUE)
  }
}

## Introductory Statistics with Rの本のデータ集がなければ勝手にインストールします。
inst("ISwR")

## library()はパッケージを読み込むコマンド。ISwRが上記の本のデータ集パッケージです。
library(ISwR)


## データフレーム作り。要は月経発来(menarche)を年齢(age)で予測するためのデータ。
## juulというデータの読み込み。どんなデータかは ?juul で読めます。
data(juul)
## menarcheという変数が0, 1になっていますがわかりにくいのでNo, Yesにかえます
juul$menarche <- factor(juul$menarche, labels = c("No","Yes"))
## subset()はデータフレームから条件でsubgroupを抜き取ります。
## ここではage<8かつage<20かつ complete.cases()をつかってmenarche欠損値でないもの。
juul.girl <- subset(juul, age > 8 & age < 20 & complete.cases(menarche))


## ROC解析のパッケージの一つpROCをインストール。
## 制作元Swiss Institute of Bioinformatics: http://web.expasy.org/pROC/
## 説明: http://www.biomedcentral.com/1471-2105/12/77
inst("pROC")

## pROCを読み込みます。
library(pROC)

## ROC解析を行います。

## まずROCオブジェクトをつくります。
## outcome ~ predictorの書式を使います。
roc.menarche.by.age <- roc(formula = menarche ~ age, data = juul.girl)

## そのまま実行すると結果が見られます。
roc.menarche.by.age

## ci()で信頼区間をつけられます。
ci(roc.menarche.by.age, of = "auc", method = "delong")  # DeLong
ci(roc.menarche.by.age, of = "auc", method = "boot")    # bootstrap


## of = "thresholds" をつけると任意のカットオフでの感度特異度の95%信頼区間がだせます。
ci(roc.menarche.by.age, of = "thresholds")



## plot()するとグラフになります。
## 何も考えずに書くとpROCはX軸を一般的ではない(けど確かにわかりやすい)方法でかきます。
## legacy.axes = TRUE を与えるとX軸が 1 - specificityになります。
plot(roc.menarche.by.age, legacy.axes = TRUE)
